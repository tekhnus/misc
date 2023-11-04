package main

import (
	"bufio"
	"context"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/signal"
)

func main() {
	err := Main()
	if err != nil {
		os.Exit(1)
	}
	os.Exit(0)
}

func Main() error {
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

	log.Println("Process started:", os.Args)

	cmd := "manager"
	args := os.Args[1:]

	if len(args) > 0 {
		switch args[0] {
		case "manager", "log-server":
			cmd = args[0]
			args = args[1:]
		}
	}

	switch cmd {
	case "manager":
		err = Manager(args, ctx)
	case "log-server":
		err = LogServer(args, ctx)
	default:
		err = fmt.Errorf("Unknown command")
	}

	return err
}

func Manager(args []string, ctx context.Context) error {
	return nil
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
