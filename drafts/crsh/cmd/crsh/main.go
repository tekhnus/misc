package main

import (
	"bufio"
	"context"
	_ "embed"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

//go:embed git-head.txt
var Version string

func main() {
	err := Main()
	if err != nil {
		os.Exit(1)
	}
	os.Exit(0)
}

func Main() error {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile | log.Lmsgprefix)

	logServer, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		log.SetOutput(io.Discard)
	} else {
		defer logServer.Close()
		log.SetOutput(logServer)
	}

	signals := make(chan os.Signal, 16)
	signal.Notify(signals, os.Interrupt)

	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		signal := <-signals
		log.Println("Received a signal", signal)
		cancel()
	}()

	cmdline := strings.Join(os.Args, " ")
	log.SetPrefix(fmt.Sprintf("%18s: ", cmdline))

	log.Println("Process started")

	var cmd string
	var args []string

	if len(os.Args) > 1 {
		cmd = os.Args[1]
		args = os.Args[2:]
	} else {
		cmd = "manager"
	}

	switch cmd {
	case "manager":
		err = ManagerMain(args, ctx)
	case "shell":
		err = ShellMain(args, ctx)
	case "ssh":
		err = SSHMain(args, ctx)
	case "log-server":
		err = LogServerMain(args, ctx)
	default:
		err = fmt.Errorf("Unknown command: %s", cmd)
	}

	if err != nil {
		log.Println(err)
	}

	return err
}

func ManagerMain(args []string, ctx context.Context) error {
	name := RandomName()

	for {
		shell, err := MakeShell(name)
		if err != nil {
			return err
		}
		name, err = HandleShell(shell)
		if err != nil {
			return err
		}
		if name == "" {
			break
		}
	}

	log.Println("Exiting")
	return nil
}

func HandleShell(shell Shell) (string, error) {
	defer func() {
		for shell.Out != nil || shell.Done != nil {
			select {
			case _, ok := <-shell.Out:
				if !ok {
					shell.Out = nil
				}
			case _, ok := <-shell.Done:
				if !ok {
					shell.Done = nil
				}
			}
		}
	}()

	for shell.Out != nil && shell.Done != nil {
		select {
		case msg, ok := <-shell.Out:
			if !ok {
				shell.Out = nil
				break
			}
			switch msg.Type {
			case "input":
				if strings.HasPrefix(msg.Payload, "\\") {
					tokens := strings.Split(msg.Payload, " ")
					switch tokens[0] {
					case "\\go":
						if len(tokens) != 2 {
							shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
							break
						}
						log.Println("Sending an exit message to current shell")
						shell.In.Encode(Message{Type: "execute", Payload: "exit"})
						log.Println("Stopping the shell")
						name := tokens[1]
						return name, nil
					case "\\detach":
						if len(tokens) != 1 {
							shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
							break
						}
						log.Println("Detaching from current shell")
						err := shell.Detach()
						return "", err
					default:
						shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
					}
				} else {
					shell.In.Encode(Message{Type: "execute", Payload: msg.Payload})
				}
			default:
				return "", fmt.Errorf("Unknown message type: %s", msg.Type)
			}
		case err, ok := <-shell.Done:
			if !ok {
				shell.Done = nil
				break
			}
			log.Println("Shell command finished:", err)
			return "", err
		}
	}

	return "", nil
}

type Shell = struct {
	In     *json.Encoder
	Out    chan Message
	Done   chan error
	Detach func() error
}

func MakeShell(name string) (Shell, error) {
	shellCtx, cancel := context.WithCancel(context.Background())

	var shellIn *json.Encoder
	shellOut := make(chan Message)
	shellCmdOut := make(chan error)

	log.Println("Start launching shell")
	shellCmd := MakeShellCommand(name)
	go func() {
		log.Println("Start running shell")
		err := shellCmd.Run()
		log.Println("Finish running shell:", err)
		fmt.Println("hello1")
		cancel()
		shellCmdOut <- err
		fmt.Println("hello2")
		close(shellCmdOut)
		fmt.Println("hello3")
	}()
	log.Println("Finish launching shell")

	log.Println("Start dialing shell")
	shell, err := MakeShellConnection(name, shellCtx)
	if err != nil {
		log.Println("While dialing shell:", err)
		log.Println("Waiting for shell command to complete")
		for range shellCmdOut {
		}
		return Shell{}, err
	}
	shellIn = json.NewEncoder(shell)
	go func() {
		ReadJsons(shell, shellOut)
		shell.Close()
		close(shellOut)
	}()
	log.Println("Finish dialing shell")

	detach := func() error {
		detachCmd := MakeDetachCommand(name)
		err := detachCmd.Run()
		if err != nil {
			return err
		}
		return shell.Close()
	}
	return Shell{shellIn, shellOut, shellCmdOut, detach}, nil
}

func MakeShellCommand(name string) *exec.Cmd {
	shellCmd := exec.Command("tmux", "new-session", "-A", "-s", name, "crs", "shell", name)

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	return shellCmd
}

func MakeDetachCommand(name string) *exec.Cmd {
	shellCmd := exec.Command("tmux", "detach-client", "-s", "="+name)

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	return shellCmd
}

func MakeShellConnection(name string, ctx context.Context) (net.Conn, error) {
	for {
		select {
		case <-ctx.Done():
			return nil, fmt.Errorf("Cancelled")
		default:
		}
		shell, err := net.Dial("unix", GetSocketPath(name))
		if err != nil {
			log.Println(err)
			time.Sleep(time.Second / 5)
			continue
		}
		return shell, nil
	}
}

func ShellMain(args []string, ctx context.Context) error {
	fs := flag.NewFlagSet("shell", flag.ContinueOnError)
	err := fs.Parse(args)
	if err != nil {
		return err
	}
	if fs.NArg() != 1 {
		return fmt.Errorf("Expected one argument")
	}
	name := fs.Arg(0)

	listener, err := net.Listen("unix", GetSocketPath(name))
	if err != nil {
		return err
	}
	defer listener.Close()

	sessions, err := ListSessions()
	if err != nil {
		return err
	}

	header := false
	for _, session := range sessions {
		if session == name {
			continue
		}
		if !header {
			fmt.Println("Sessions on this host:")
			header = true
		}
		fmt.Printf("- %s\n", session)
	}

	for {
		log.Println("Start accepting connection")
		manager, err := listener.Accept()
		if err != nil {
			log.Println(err)
			return err
		}
		log.Println("Finish accepting connection")
		cont, err := HandleManager(manager, ctx)
		if err != nil {
			return err
		}
		if !cont {
			return nil
		}
	}
}

func GetSocketPath(name string) string {
	return "/tmp/crsh-shell-" + name
}

func HandleManager(manager net.Conn, ctx context.Context) (bool, error) {
	defer manager.Close()

	managerIn := json.NewEncoder(manager)

	managerOut := make(chan Message)
	go func() {
		ReadJsons(manager, managerOut)
		close(managerOut)
	}()

	err := SimpleExecute("tmux set-option -g status-left-length 32")
	if err != nil {
		log.Println("While configuring tmux:", err)
	}

	stdin := bufio.NewScanner(os.Stdin)
	inputs := make(chan string)
	for {
		go func() {
			ok, _ := Prompt(stdin, inputs)
			if !ok {
				close(inputs)
			}
		}()

		select {
		case input, ok := <-inputs:
			if !ok {
				log.Println("No more input")
				return false, nil
			}
			log.Println("Received", input)
			err = managerIn.Encode(Message{Type: "input", Payload: input})
			if err != nil {
				log.Println(err)
				return false, err
			}
		case <-ctx.Done():
			return false, nil
		}

		select {
		case msg, ok := <-managerOut:
			if !ok {
				log.Println("No more messages from manager")
				return true, nil
			}
			log.Println("Received", msg)
			switch msg.Type {
			case "execute":
				if msg.Payload == "exit" {
					log.Println("Exiting")
					return false, nil
				}
				err = SimpleExecute(msg.Payload)
				if err != nil {
					fmt.Fprintln(os.Stderr, err)
				}
			default:
				return false, fmt.Errorf("Unknown message type: %s", msg.Type)
			}
		case <-ctx.Done():
			return false, nil
		}
	}
}

func SSHMain(args []string, ctx context.Context) error {
	fs := flag.NewFlagSet("shell", flag.ContinueOnError)
	err := fs.Parse(args)
	if err != nil {
		return err
	}
	if fs.NArg() < 1 {
		return fmt.Errorf("Expected one argument")
	}
	host := fs.Arg(0)

	var wg sync.WaitGroup
	defer func() {
		log.Println("Started waiting on child processes")
		wg.Wait()
		log.Println("Finished waiting on child processes")
	}()

	masterSocket := fmt.Sprintf("/tmp/crsh-ssh-%d", os.Getpid())

	masterCmd := exec.Command("ssh", "-M", "-S", masterSocket, "-N", host)
	wg.Add(1)
	go func() {
		defer wg.Done()
		log.Println("Started command:", masterCmd)
		out, err := masterCmd.CombinedOutput()
		log.Println("Finished command:", masterCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	}()

	masterExitCmd := exec.Command("ssh", "-S", masterSocket, "-O", "exit", host)
	defer func() {
		log.Println("Started command:", masterExitCmd)
		out, err := masterExitCmd.CombinedOutput()
		log.Println("Finished command:", masterExitCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	}()

	srcDir := os.ExpandEnv("$HOME/.local/share/crsh/" + Version)
	tmpDir := ".local/share/crsh/tmp"
	downloadDir := ".local/share/crsh/tmp/" + Version
	dstDir := ".local/share/crsh/" + Version

	testCmd := exec.Command(
		"ssh", "-S", masterSocket, host,
		"[", "-e", dstDir, "]")
	out, err := testCmd.CombinedOutput()
	log.Println("Finished command:", testCmd)
	log.Print("Output: ", string(out))
	log.Println("Status:", err)

	if err != nil {
		mkdirCmd := exec.Command(
			"ssh", "-S", masterSocket, host,
			"rm", "-rf", tmpDir,
			"&&", "mkdir", "-p", tmpDir)
		out, err := mkdirCmd.CombinedOutput()
		log.Println("Finished command:", mkdirCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)

		scpCmd := exec.Command(
			"scp", "-o", "ControlPath="+masterSocket,
			"-r", srcDir, host+":"+downloadDir)
		out, err = scpCmd.CombinedOutput()
		log.Println("Finished command:", scpCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)

		cpCmd := exec.Command(
			"ssh", "-S", masterSocket, host,
			"mv", downloadDir, dstDir)
		out, err = cpCmd.CombinedOutput()
		log.Println("Finished command:", cpCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	} else {
		log.Println("Skipping transfer")
	}

	executable := dstDir + "/linux/crsh"
	shellCmd := exec.Command(
		"ssh", "-S", masterSocket, "-t", host,
		executable)
	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	err = shellCmd.Run()
	log.Println("Finished command:", shellCmd)
	log.Println("Status:", err)

	time.Sleep(1 * time.Second)

	return nil
}

func ListSessions() ([]string, error) {
	var sessions []string
	sockets, err := filepath.Glob("/tmp/crsh-shell-*")
	if err != nil {
		return nil, err
	}
	for _, sock := range sockets {
		name, ok := strings.CutPrefix(sock, "/tmp/crsh-shell-")
		if !ok {
			return nil, fmt.Errorf("Unexpected problem")
		}
		sessions = append(sessions, name)
	}
	return sessions, nil
}

func SimpleExecute(stmt string) error {
	args := strings.Split(stmt, " ")
	cmd := exec.Command(args[0], args[1:]...)

	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

func Prompt(src *bufio.Scanner, dst chan string) (bool, error) {
	fmt.Print("> ")
	ok := src.Scan()
	if ok {
		dst <- src.Text()
	}
	return ok, src.Err()
}

func LogServerMain(args []string, ctx context.Context) error {
	listener, err := net.Listen("tcp", "localhost:5678")
	if err != nil {
		return err
	}
	defer listener.Close()

	conns := make(chan net.Conn)
	go Accept(listener, conns)

	lines := make(chan string)
	go PrintLines(lines)
	for {
		select {
		case conn := <-conns:
			defer conn.Close()
			go ReadLines(conn, lines)
		case <-ctx.Done():
			log.Println("Cancelling")
			return nil
		}
	}
}

func Accept(listener net.Listener, dst chan net.Conn) error {
	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}
		dst <- conn
	}
}

func PrintLines(lines chan string) {
	for line := range lines {
		fmt.Println(line)
	}
}

func ReadLines(conn net.Conn, lines chan string) error {
	reader := bufio.NewReader(conn)
	for {
		line, _, err := reader.ReadLine()
		if err != nil {
			log.Println(err)
			return err
		}
		fmt.Println(string(line))
	}
}

func ReadJsons(conn net.Conn, dst chan Message) error {
	reader := json.NewDecoder(conn)
	for {
		var msg Message
		err := reader.Decode(&msg)
		if err != nil {
			log.Println(err)
			return err
		}
		dst <- msg
	}
}

func RandomName() string {
	words := []string{
		"ocean", "mountain", "cat", "computer", "river",
		"painting", "novel", "piano", "lawyer", "bacteria",
		"galaxy", "equation", "volcano", "poetry",
		"theater", "neuron", "atom", "eclipse", "typhoon",
		"quasar", "jazz", "chess", "robot", "cipher",
		"sonnet", "monsoon", "kaleidoscope", "harmony", "jungle",
	}
	var result []string
	for i := 0; i < 3; i++ {
		result = append(result, words[rand.Intn(len(words))])
	}
	return strings.Join(result, "-")
}

type Message = struct {
	Type    string
	Payload string
}
