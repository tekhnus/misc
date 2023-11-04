package main

import (
	"bufio"
	"fmt"
	"io"
	"log"
	"net"
	"os"
)

func main() {
	logServer, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		log.SetOutput(io.Discard)
	} else {
		defer logServer.Close()
		log.SetOutput(logServer)
	}

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

	var res error
	switch cmd {
	case "manager":
		res = Manager(args)
	case "log-server":
		res = LogServer(args)
	default:
		log.Fatal("Unknown command")
	}

	if res != nil {
		log.Fatal(res)
	}
}

func Manager(args []string) error {
	return nil
}

func LogServer(args []string) error {
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
			go func() {
				defer conn.Close()
				ReadLines(conn, lines)
			}()
			go ReadLines(conn, lines)
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
