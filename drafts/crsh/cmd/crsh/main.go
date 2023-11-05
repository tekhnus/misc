package main

import (
	"bufio"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"strings"
)

func main() {
	err := Main()
	if err != nil {
		os.Exit(1)
	}
	os.Exit(0)
}

func Main() error {
	log.SetFlags(log.Ldate | log.Ltime | log.Lmsgprefix)

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
		err = Manager(args, ctx)
	case "shell":
		err = Shell(args, ctx)
	case "log-server":
		err = LogServer(args, ctx)
	default:
		err = fmt.Errorf("Unknown command: %s", cmd)
	}

	if err != nil {
		log.Println(err)
	}

	return err
}

func Manager(args []string, ctx context.Context) error {
	listener, err := net.Listen("unix", "/tmp/crsh-manager")
	if err != nil {
		return err
	}
	defer listener.Close()

	shellCmd := exec.Command("crsh", "shell")

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	shellCmd.Start()
	defer shellCmd.Wait()

	log.Println("Start accepting connection")
	shell, err := listener.Accept()
	if err != nil {
		log.Println(err)
		return err
	}
	defer shell.Close()
	log.Println("Finish accepting connection")

	shellOut := make(chan Message)
	go func() {
		ReadJsons(shell, shellOut)
		close(shellOut)
	}()

	shellIn := json.NewEncoder(shell)

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
				shellIn.Encode(Message{Type: "execute", Payload: msg.Payload})
			default:
				return fmt.Errorf("Unknown message type %s", msg.Type)
			}
		}
	}
}

func Shell(args []string, ctx context.Context) error {
	log.Println("Start dialing manager")
	manager, err := net.Dial("unix", "/tmp/crsh-manager")
	if err != nil {
		log.Println(err)
		return err
	}
	defer manager.Close()
	log.Println("Finish dialing manager")

	managerIn := json.NewEncoder(manager)

	managerOut := make(chan Message)
	go func() {
		ReadJsons(manager, managerOut)
		close(managerOut)
	}()

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

func LogServer(args []string, ctx context.Context) error {
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

type Message struct {
	Type    string
	Payload string
}
