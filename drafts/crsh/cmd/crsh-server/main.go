package main

import (
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/exec"
	"time"
)

func main() {
	if len(os.Args) <= 1 {
		log.Fatal("Not enough arguments")
	}

	cmd := os.Args[1]
	args := os.Args[2:]

	var err error

	switch cmd {
	case "echo":
		err = echo(args)
	case "manager":
		err = manager(args)
	default:
		log.Fatal("Unknown command")
	}

	if err != nil {
		log.Fatal(err)
	}
}

func echo(args []string) error {
	listener, err := net.Listen("tcp", "localhost:5679")
	if err != nil {
		return err
	}
	defer listener.Close()

	conn, err := listener.Accept()
	if err != nil {
		return err
	}
	defer conn.Close()

	dec := json.NewDecoder(conn)
	for {
		var msg map[string]string
		err = dec.Decode(&msg)
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		fmt.Printf("received: %s\n", msg)
	}
	return nil
}

func manager(args []string) error {
	listener, err := net.Listen("tcp", "localhost:5678")
	if err != nil {
		return err
	}
	defer listener.Close()

	conn, err := listener.Accept()
	if err != nil {
		return err
	}
	defer conn.Close()

	cmd := exec.Command("crsh-server", "echo")
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err = cmd.Start()
	if err != nil {
		return err
	}
	defer cmd.Wait()

	log.Println("Starting dialing shell")
	cmdconn, err := net.Dial("tcp", "localhost:5679")
	for err != nil {
		time.Sleep(time.Second / 5)
		cmdconn, err = net.Dial("tcp", "localhost:5679")
	}
	defer cmdconn.Close()
	log.Println("Ending dialing shell")

	go func() { io.Copy(conn, cmdconn) }()
	io.Copy(cmdconn, conn)

	return nil
}
