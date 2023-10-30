package main

import (
	"bufio"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/url"
	"os"
	"os/exec"
	"os/signal"
	"os/user"
	"path/filepath"
	"strings"
	"time"
)

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lmsgprefix)

	logger, err := net.Dial("tcp", "localhost:5679")
	if err == nil {
		defer logger.Close()
		log.SetOutput(logger)
	} else {
		log.Println("While trying to connect to logserver:", err)
	}
	ctx, cancel := context.WithCancel(context.Background())

	signals := make(chan os.Signal, 1)
	signal.Notify(signals, os.Interrupt)

	go func() {
		<-signals
		log.Println("Received an interrupt signal")
		cancel()
		time.Sleep(1 * time.Second)
		log.Fatal("Self-terminating by timeout")
	}()

	if len(os.Args) <= 1 {
		log.Fatal("Not enough arguments")
	}

	cmd := os.Args[1]
	args := os.Args[2:]

	switch cmd {
	case "echo":
		err = echo(args, ctx)
	case "manager":
		err = manager(args, ctx)
	case "ssh":
		err = ssh(args, ctx)
	case "logserver":
		err = logserver(args, ctx)
	case "scratch":
		cmd := exec.Command("abduco", "-A", "scratch", "cat")
		cmd.Stdin = os.Stdin
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr

		err = cmd.Start()
		if err != nil {
			break
		}
		cmd.Wait()
	default:
		log.Fatal("Unknown command")
	}

	log.Println("Terminating the command")
	if err != nil {
		log.Fatal(err)
	}
}

func echo(args []string, ctx context.Context) error {
	fset := flag.NewFlagSet("echo", flag.ExitOnError)
	name := fset.String("name", "[no name]", "name for the logger")
	fset.Parse(args)
	if fset.NArg() < 1 {
		return errors.New("expected a url")
	}
	log.SetPrefix(fmt.Sprintf("%18s ", "echo " + *name))
	addr := fset.Arg(0)
	url, err := url.Parse(addr)
	if err != nil {
		return err
	}
	if url.Scheme == "unix" {
		log.Println("trying to unlink the socket first")
		err := os.Remove(url.Path)
		if err != nil {
			log.Println(err)
		}
	}
	listener, err := net.Listen(url.Scheme, url.Host+url.Path)
	if err != nil {
		return err
	}
	defer listener.Close()

	connections := make(chan net.Conn)
	go func() {
		for {
			log.Println("Listening for a connection")
			conn, err := listener.Accept()
			if err != nil {
				log.Fatal(err)
			}
			log.Println("Accepted a connection")
			connections <- conn
		}
	}()
	for {
		select {
		case conn := <-connections:
			exit, _ := echoLoop(conn, ctx)
			if exit {
				log.Println("Exiting because was told to do so")
				return nil
			}
		case <-ctx.Done():
			log.Println("Context is done")
			return nil
		}
	}
	log.Println("exiting")
	return nil
}

func echoLoop(conn net.Conn, ctx context.Context) (bool, error) {
	defer conn.Close()
	msgs := make(chan map[string]string)
	go readMessages(conn, msgs)
	enc := json.NewEncoder(conn)
	for {
		select {
		case msg := <-msgs:
			log.Println("Received a message", msg)
			fmt.Printf("received: %s\n", msg)
			if msg["type"] == "cmd" {
				if msg["cmd"] == "exit" {
					enc.Encode(map[string]string{"type": "status", "status": "exiting"})
					return true, nil
				}
			} else if msg["type"] == "end" {
				return false, nil
			} else {
				log.Panicf("Unsupported message type: %s\n", msg["type"])
			}
		case <-ctx.Done():
			log.Println("Context is done")
			return true, nil
		}
	}
}

func ssh(args []string, ctx context.Context) error {
	log.SetPrefix(fmt.Sprintf("%18s ", "ssh"))

	fset := flag.NewFlagSet("ssh", flag.ExitOnError)
	fset.Parse(args)
	if fset.NArg() < 3 {
		return errors.New("expected an address, a host and a command")
	}
	addr := fset.Arg(0)
	host := fset.Arg(1)
	cmd := fset.Args()[2:]

	url, err := url.Parse(addr)
	if err != nil {
		return err
	}
	socket := url.Host + url.Path

	statuses := make(chan struct {*exec.Cmd; string; error})

	masterSocket := fmt.Sprintf("/tmp/crsh-ssh-socket%d", os.Getpid())
	defer os.Remove(masterSocket)

	masterCmd := exec.Command("ssh", "-M", "-S", masterSocket, "-N", host)
	go func() {
		outp, err := masterCmd.CombinedOutput()
		statuses <- struct{*exec.Cmd; string; error}{masterCmd, string(outp), err}
	}()
	defer func() {
		if masterCmd.Process != nil {
			masterCmd.Process.Kill()
		}
	}()

	usr, _ := user.Current()
	dir := usr.HomeDir
	remoteBinDir := "/home/" + usr.Username + "/.local/bin"
	rmBinArgs := []string{"-S", masterSocket, host, "rm", "-f", remoteBinDir + "/" + "crsh*"}
	log.Println("ssh rm args", rmBinArgs)
	rmcmd := exec.Command("ssh", rmBinArgs...)
	go func() {
		outp, err := rmcmd.CombinedOutput()
		statuses <- struct{*exec.Cmd; string; error}{rmcmd, string(outp), err}
	}()

	status := <- statuses
	switch status.Cmd {
	case masterCmd:
		log.Println("master finished:", status.error, status.string)
		return fmt.Errorf("master finished unexpectedly")
	case rmcmd:
		if status.error != nil {
			log.Println("rm failed:", status.error, status.string)
			return status.error
		}
	default:
		log.Panicln("unexpected command")
	}

	srcDir := filepath.Join(dir, ".local", "share", "crsh", "linux")
	srcF, err := os.Open(srcDir)
	if err != nil {
		return err
	}
	defer srcF.Close()
	entries, err := srcF.Readdirnames(0)
	if err != nil {
		return err
	}

	scpArgs := []string{"-o", "ControlPath=" + masterSocket}
	for _, entry := range entries {
		scpArgs = append(scpArgs, filepath.Join(srcDir, entry))
	}
	dst := host + ":" + remoteBinDir
	scpArgs = append(scpArgs, dst)

	scpcmd := exec.Command("scp", scpArgs...)
	go func() {
		outp, err := scpcmd.CombinedOutput()
		statuses <- struct{*exec.Cmd; string; error}{scpcmd, string(outp), err}
	}()
	status = <- statuses
	switch status.Cmd {
	case masterCmd:
		log.Println("master finished:", status.error, status.string)
		return fmt.Errorf("master finished unexpectedly")
	case scpcmd:
		if status.error != nil {
			log.Println("scp failed:", status.error, status.string)
			return status.error
		}
	default:
		log.Panicln("unexpected command")
	}

	sshArgs := []string{"-t", host}
	sshArgs = append(sshArgs, cmd...)
	log.Println("ssh args", sshArgs)
	comd := exec.Command("ssh", sshArgs...)
	comd.Stdin = os.Stdin
	comd.Stdout = os.Stdout
	comd.Stderr = os.Stderr

	err = comd.Start()
	if err != nil {
		return err
	}
	defer func() {
		if comd.Process != nil {
			comd.Process.Kill()
		}
	}()
	go func() {
		statuses <- struct{*exec.Cmd; string; error}{comd, "", comd.Wait()}
	}()

	// FIXME
	// This is needed to give the server the time to create the socket.
	// If the forwarding command starts before the remote socket is created,
	// then the local socket will start in an corrupted state (it will accept connections)
	// but it will close immediately.
	time.Sleep(time.Second * 3)

	log.Println("trying to unlink the socket first")
	err = os.Remove(socket)
	if err != nil {
		log.Println(err)
	}

	fwdArgs := []string{"-N", "-o", "ExitOnForwardFailure=yes", "-L", socket + ":" + socket, host}
	log.Println("sshfwd args", fwdArgs)
	fwdcomd := exec.Command("ssh", fwdArgs...)
	defer func() {
		if fwdcomd.Process != nil {
			fwdcomd.Process.Kill()
		}
	}()
	go func() {
		outp, err := fwdcomd.CombinedOutput()
		statuses <- struct{*exec.Cmd; string; error}{fwdcomd, string(outp), err}
	}()

	status = <-statuses
	return status.error
}

func readMessages(conn net.Conn, outp chan map[string]string) {
	dec := json.NewDecoder(conn)
	for {
		var msg map[string]string
		err := dec.Decode(&msg)
		if err != nil {
			var isEof string
			if err == io.EOF {
				isEof = "1"
			} else {
				isEof = "0"
			}
			outp <- map[string]string{"type": "end", "is_eof": isEof, "error": err.Error()}
			break
		}
		outp <- msg
	}
}

func manager(args []string, ctx context.Context) error {
	log.SetPrefix(fmt.Sprintf("%18s ", fmt.Sprintf("manager %d", os.Getpid())))

	fset := flag.NewFlagSet("manager", flag.ExitOnError)
	initname := fset.String("name", "", "initial session name")
	inithost := fset.String("host", "localhost", "initial session host")
	fset.Parse(args)
	if fset.NArg() < 1 {
		return errors.New("expected a url")
	}
	if *inithost == "localhost" {
		*inithost = ""
	}
	addr := fset.Arg(0)
	murl, err := url.Parse(addr)
	if err != nil {
		return err
	}
	listener, err := net.Listen(murl.Scheme, murl.Host+murl.Path)
	if err != nil {
		return err
	}
	defer listener.Close()

	client, err := listener.Accept()
	if err != nil {
		return err
	}
	defer client.Close()

	clientMsgs := make(chan map[string]string)
	go readMessages(client, clientMsgs)

	name := ""
	host := ""

	var serverProcessWaiter chan error
	var server net.Conn
	var toServer chan map[string]string
	var serverDone chan error

	closeView := func() {
		var cmd []string
		if host != "" {
			cmd = append(cmd, "ssh", host)
		}
		cmd = append(cmd, "tmux", "detach-client", "-s", name)
		err := exec.Command(cmd[0], cmd[1:]...).Run()
		if err != nil {
			log.Println("detach command error", err)
		}
	}
	serve := func() {
		defer func() { serverDone <- nil }()
		defer log.Println("sending the done signal")
		defer log.Println("waiting server process")
		defer closeView()
		defer log.Println("closing view")
		defer server.Close()
		defer log.Println("closing server")

		fromServer := make(chan map[string]string)
		go readMessages(server, fromServer)

		enc := json.NewEncoder(server)
		for {
			select {
			case msg, ok := <-toServer:
				log.Println("Received from toserver channel", msg)
				if !ok {
					log.Println("channel is closed, so exiting")
					return
				}
				err := enc.Encode(msg)
				if err != nil {
					log.Println("error while encoding message to server, so exiting", err)
					return
				}
			case msg, ok := <-fromServer:
				log.Println("Received from fromserver channel", msg)
				if !ok {
					log.Println("channel is closed, so exiting")
					return
				}
				if msg["type"] == "status" && msg["status"] == "exiting" {
					log.Println("client said it's exiting, so exiting")
					return
				}
				log.Println("doing nothing with client message")
			case err, ok := <-serverProcessWaiter:
				log.Println("Received from serverwaiter channel", err)
				if !ok {
					log.Println("channel is closed, so exiting")
					return
				}
				return
			}
		}
	}

	for {
		if name == "" && host == "" {
			log.Println("connecting to initial session")
			name = *initname
			host = *inithost
		}
		asock := filepath.Join("/tmp", name)
		aurl := "unix://" + asock
		var cmd []string
		if host != "" {
			cmd = append(cmd, "crsh-server", "ssh", aurl, host)
		}
		cmd = append(cmd, "tmux", "new-session", "-A", "-s", name, "crsh-server", "echo", "-name", name, aurl)
		_, serverProcessWaiter, server, err = startSession(
			cmd,
			aurl)
		if err != nil {
			log.Println("error while trying to open session", err)
			log.Println("switching back to default session")
			name = ""
			host = ""
			continue
		}
		toServer = make(chan map[string]string)
		serverDone = make(chan error)
		log.Println("connected to session")
		go serve()
	Loop:
		for {
			select {
			case msg := <-clientMsgs:
				if msg["type"] == "end" {
					log.Println("Received end message")
					break
				}
				if msg["type"] != "cmd" {
					log.Panicf("Unknown message type: %s\n", msg["type"])
				}
				parsedMsg := strings.Split(msg["cmd"], " ")
				if parsedMsg[0] == "\\ssh" {
					log.Println("sending the exit request to the server")
					toServer <- map[string]string{"type": "cmd", "cmd": "exit"}
					close(toServer)
					log.Println("waiting for the server")
					<-serverDone
					log.Println("wait done")
					if len(parsedMsg) == 2 {
						host = parsedMsg[1]
					} else {
						return errors.New("Expected one or two arguments")
					}
					break Loop
				} else if parsedMsg[0] == "\\detach" {
					log.Println("clising the server control")
					close(toServer)
					log.Println("waiting for the server")
					<-serverDone
					log.Println("wait done")
					return nil
				} else if parsedMsg[0] == "\\new" {
					// FIXME: we need to close the server temporarily
					// because if we use remote control while the tmux is running
					// it goes wild.
					log.Println("clising the server control")
					close(toServer)
					log.Println("waiting for the server")
					<-serverDone
					log.Println("wait done")

					newname := name + "1"
					hostcopy := host
					if hostcopy == "" {
						hostcopy = "localhost"
					}
					err := SimpleRun(fmt.Sprintf(`kitty @ launch --env PATH=%s --type tab crsh -name %s -host %s`, os.Getenv("PATH"), newname, hostcopy))
					if err != nil {
						log.Println("error while opening the tab: ", err)
					}

					break Loop
				} else {
					log.Println("forwarding the message", msg)
					toServer <- msg
					log.Println("forwarded the message")
				}
			case <-serverDone:
				log.Println("the server is done")
				return nil
			}
		}
	}

	log.Println("exiting")
	return nil
}

func SimpleRun(comm string) error {
	args := strings.Split(comm, " ")
	log.Println("Executing", args)
	cmd := exec.Command(args[0], args[1:]...)
	cmd.Stdin = nil
	cmd.Stdout = nil
	cmd.Stderr = nil
	err := cmd.Run()
	log.Println("Finished execution")
	return err
}

func startSession(cmdline []string, addr string) (*exec.Cmd, chan error, net.Conn, error) {
	url, err := url.Parse(addr)
	if err != nil {
		return nil, nil, nil, err
	}
	if len(cmdline) == 0 {
		return nil, nil, nil, errors.New("cmdline cannot be empty")
	}
	cmd := exec.Command(cmdline[0], cmdline[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err = cmd.Start()
	if err != nil {
		return nil, nil, nil, err
	}
	waiter := make(chan error)
	go func() {
		waiter <- cmd.Wait()
	}()

	log.Println("Starting dialing shell", addr, url.Scheme, url.Host+url.Path)
	cmdconn, err := net.Dial(url.Scheme, url.Host+url.Path)
	for err != nil {
		log.Println(err)
		select {
		case err := <-waiter:
			log.Println("The server process stopped while dialing")
			return nil, nil, nil, err
		default:
			time.Sleep(time.Second / 5)
			cmdconn, err = net.Dial(url.Scheme, url.Host+url.Path)
		}
	}
	log.Println("Ending dialing shell")

	return cmd, waiter, cmdconn, nil
}

func logserver(args []string, ctx context.Context) error {
	log.SetPrefix(fmt.Sprintf("%18s ", "logserver"))

	fset := flag.NewFlagSet("logserver", flag.ExitOnError)
	remotes := fset.String("remotes", "", "remote hosts")
	fset.Parse(args)

	statuses := make(chan error)
	var remoteList []string
	if *remotes != "" {
		remoteList = strings.Split(*remotes, ",")
	}
	for _, remote := range remoteList {
		fwdArgs := []string{"-N", "-o", "ExitOnForwardFailure=yes", "-R", "5679:localhost:5679", remote}
		log.Println("sshfwd args", fwdArgs)
		fwdcomd := exec.Command("ssh", fwdArgs...)
		err := fwdcomd.Start()
		if err != nil {
			return err
		}
		defer fwdcomd.Process.Kill()
		go func() {
			statuses <- fwdcomd.Wait()
		}()
	}

	listener, err := net.Listen("tcp", "localhost:5679")
	if err != nil {
		return err
	}
	defer listener.Close()

	go func() {
		defer func() { statuses <- errors.New("listener stopped") }()
		for {
			conn, err := listener.Accept()
			if err != nil {
				return
			}
			go func() {
				defer conn.Close()
				reader := bufio.NewReader(conn)
				for {
					line, _, err := reader.ReadLine()
					if err != nil {
						break
					}
					fmt.Printf("%s\n", line)
				}
			}()
		}
	}()

	log.Println("Waiting while one of the forwarding processes stops")
	select {
	case err = <- statuses:
		log.Println("Forwarding process stopped:", err)
		return err
	case <- ctx.Done():
		log.Println("the context is done")
		return nil
	}
}
