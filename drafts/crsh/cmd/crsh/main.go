package main

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"strings"
	"sync"
	"time"
)

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
	var wg sync.WaitGroup
	name := RandomName()

	for {
		log.Println("Start waiting for previous shell")
		wg.Wait()
		log.Println("Finish waiting for previous shell")

		shell, err := MakeShell(name)
		if err != nil {
			return err
		}
		shellIn := make(chan Message)
		go func() {
			for message := range shellIn {
				err := shell.In.Encode(message)
				if err != nil {
					return
				}
			}
		}()

		shellOutFiltered := make(chan Message)

		wg.Add(1)
		go func() {
			defer wg.Done()
			HandleShell(shellIn, shellOutFiltered, shell.Done)
		}()

		exit := true
	Shell:
		for msg := range shell.Out {
			if msg.Type == "input" && strings.HasPrefix(msg.Payload, "\\") {
				tokens := strings.Split(msg.Payload, " ")
				switch tokens[0] {
				case "\\go":
					if len(tokens) != 2 {
						shellIn <- Message{Type: "execute", Payload: "echo Wrong command"}
						continue Shell
					}
					log.Println("Sending an exit message to current shell")
					shellIn <- Message{Type: "execute", Payload: "exit"}
					log.Println("Stopping the shell")
					exit = false
					name = tokens[1]
					break Shell
				default:
					shellIn <- Message{Type: "execute", Payload: "echo Wrong command"}
				}
			} else {
				shellOutFiltered <- msg
			}
		}

		log.Println("Closing shell handler")
		close(shellOutFiltered)
		if exit {
			break
		}
	}

	log.Println("Exiting")
	return nil
}

func HandleShell(shellIn chan Message, shellOut chan Message, shellCmdOut chan error) error {
	defer func() {
		log.Println("Start ensuring closed shell status")
		for range shellCmdOut {
		}
		log.Println("Finish ensuring closed shell status")
	}()
	defer func() {
		log.Println("Start ensuring closed shell connection")
		for range shellOut {
		}
		log.Println("Finish ensuring closed shell connection")
	}()

	for {
		select {
		case msg, ok := <-shellOut:
			if !ok {
				log.Println("No more shell messages")
				return nil
			}
			log.Println("Received", msg)
			switch msg.Type {
			case "input":
				shellIn <- Message{Type: "execute", Payload: msg.Payload}
			default:
				return fmt.Errorf("Unknown message type %s", msg.Type)
			}
		case err, ok := <-shellCmdOut:
			if !ok {
				return fmt.Errorf("Channel closed unexpectedly")
			}
			log.Println("Shell command finished:", err)
			return err
		}
	}
}

type Shell = struct {
	In   *json.Encoder
	Out  chan Message
	Done chan error
}

func MakeShell(name string) (Shell, error) {
	var shellIn *json.Encoder
	shellOut := make(chan Message)
	shellCmdOut := make(chan error)

	log.Println("Start launching shell")
	shellCmd := MakeShellCommand(name)
	go func() {
		shellCmdOut <- shellCmd.Run()
		close(shellCmdOut)
	}()
	log.Println("Finish launching shell")

	log.Println("Start dialing shell")
	shell, err := MakeShellConnection()
	if err != nil {
		log.Println(err)
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

	return Shell{shellIn, shellOut, shellCmdOut}, nil
}

func MakeShellCommand(name string) *exec.Cmd {
	shellCmd := exec.Command("tmux", "new-session", "-A", "-s", name, "crsh", "shell")

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	return shellCmd
}

func MakeShellConnection() (net.Conn, error) {
	for {
		shell, err := net.Dial("unix", "/tmp/crsh-manager")
		if err != nil {
			log.Println(err)
			time.Sleep(time.Second / 5)
			continue
		}
		return shell, nil
	}
}

func ShellMain(args []string, ctx context.Context) error {
	listener, err := net.Listen("unix", "/tmp/crsh-manager")
	if err != nil {
		return err
	}
	defer listener.Close()

	log.Println("Start accepting connection")
	manager, err := listener.Accept()
	if err != nil {
		log.Println(err)
		return err
	}
	defer manager.Close()
	log.Println("Finish accepting connection")

	managerIn := json.NewEncoder(manager)

	managerOut := make(chan Message)
	go func() {
		ReadJsons(manager, managerOut)
		close(managerOut)
	}()

	err = SimpleExecute("tmux set-option -g status-left-length 32")
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
				return nil
			}
			log.Println("Received", input)
			err = managerIn.Encode(Message{Type: "input", Payload: input})
			if err != nil {
				log.Println(err)
				return err
			}
		case <-ctx.Done():
			return nil
		}

		select {
		case msg, ok := <-managerOut:
			if !ok {
				log.Println("No more messages from manager")
				return nil
			}
			log.Println("Received", msg)
			switch msg.Type {
			case "execute":
				if msg.Payload == "exit" {
					log.Println("Exiting")
					return nil
				}
				err = SimpleExecute(msg.Payload)
				if err != nil {
					fmt.Fprintln(os.Stderr, err)
				}
			default:
				return fmt.Errorf("Unknown message type: %s", msg.Type)
			}
		case <-ctx.Done():
			return nil
		}
	}
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
