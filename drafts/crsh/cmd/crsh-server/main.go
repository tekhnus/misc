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
		err = logserver()
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
	log.SetPrefix(fmt.Sprintf("%12s ", "echo "+*name))
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

	usr, _ := user.Current()
	dir := usr.HomeDir

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

	var scpArgs []string
	for _, entry := range entries {
		scpArgs = append(scpArgs, filepath.Join(srcDir, entry))
	}
	dst := host + ":" + "/home/" + usr.Username + "/.local/bin"
	scpArgs = append(scpArgs, dst)

	out, err := exec.Command("scp", scpArgs...).CombinedOutput()
	fmt.Println(string(out))
	if err != nil {
		return err
	}

	sshArgs := []string{"-L", socket + ":" + socket, "-t", host}
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
	return comd.Wait()
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
	log.SetPrefix(fmt.Sprintf("%12s ", "manager"))

	fset := flag.NewFlagSet("manager", flag.ExitOnError)
	fset.Parse(args)
	if fset.NArg() < 1 {
		return errors.New("expected a url")
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

	var serverProcess *exec.Cmd
	var server net.Conn
	var toServer chan map[string]string
	var serverDone chan error

	closeView := func() {
		defer exec.Command("tmux", "detach-client", "-s", name).Run()
	}
	serve := func() {
		defer func() { serverDone <- nil }()
		defer serverProcess.Wait()
		defer closeView()
		defer server.Close()

		fromServer := make(chan map[string]string)
		go readMessages(server, fromServer)

		enc := json.NewEncoder(server)
		for {
			select {
			case msg, ok := <-toServer:
				if !ok {
					return
				}
				err := enc.Encode(msg)
				if err != nil {
					return
				}
			case msg, ok := <-fromServer:
				if !ok {
					return
				}
				log.Println("Received from server", msg)
				if msg["type"] == "status" && msg["status"] == "exiting" {
					return
				}
			}
		}
	}

	for {
		if name == "" && host == "" {
			log.Println("connecting to default session")
			name = "default"
		}
		asock := filepath.Join(os.TempDir(), name)
		aurl := "unix://" + asock
		serverProcess, server, err = startSession(
			[]string{"tmux", "new-session", "-A", "-s", name, "crsh-server", "echo", "-name", name, aurl},
			aurl)
		if err != nil {
			return err
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
				if parsedMsg[0] == "\\open" {
					log.Println("clising the server control")
					close(toServer)
					log.Println("waiting for the server")
					<-serverDone
					log.Println("wait done")
					if len(parsedMsg) == 2 {
						name = parsedMsg[1]
					} else if len(parsedMsg) == 3 {
						host = parsedMsg[1]
						name = parsedMsg[2]
					} else {
						return errors.New("Expected one or two arguments")
					}
					break Loop
				} else if parsedMsg[0] == "\\quit" {
					log.Println("clising the server control")
					close(toServer)
					log.Println("waiting for the server")
					<-serverDone
					log.Println("wait done")
					return nil
				} else {
					log.Println("forwarding the message", msg)
					toServer <- msg
					log.Println("forwarded the message")
				}
			case <-serverDone:
				log.Println("the server is done")
				name = ""
				host = ""
				break Loop
			}
		}
	}

	log.Println("exiting")
	return nil
}

func startSession(cmdline []string, addr string) (*exec.Cmd, net.Conn, error) {
	url, err := url.Parse(addr)
	if err != nil {
		return nil, nil, err
	}
	if len(cmdline) == 0 {
		return nil, nil, errors.New("cmdline cannot be empty")
	}
	cmd := exec.Command(cmdline[0], cmdline[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err = cmd.Start()
	if err != nil {
		return nil, nil, err
	}

	log.Println("Starting dialing shell", addr, url.Scheme, url.Host+url.Path)
	cmdconn, err := net.Dial(url.Scheme, url.Host+url.Path)
	for err != nil {
		log.Println(err)
		time.Sleep(time.Second / 5)
		cmdconn, err = net.Dial(url.Scheme, url.Host+url.Path)
	}
	log.Println("Ending dialing shell")

	return cmd, cmdconn, nil
}

func logserver() error {
	listener, err := net.Listen("tcp", "localhost:5679")
	if err != nil {
		return err
	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
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
}
