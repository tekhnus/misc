package main

import (
	"encoding/json"
	"errors"
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
	dec := json.NewDecoder(conn)

	cmd, cmdconn, err := startSession(
		[]string{"crsh-server", "echo"},
		"localhost:5679")
	if err != nil {
		return err
	}
	enc := json.NewEncoder(cmdconn)
	for {
		go func() { io.Copy(conn, cmdconn) }()

		for {
			var msg map[string]string
			err = dec.Decode(&msg)
			if err == io.EOF {
				log.Println("EOF")
				break
			}
			if err != nil {
				return err
			}
			if msg["cmd"] == "/open" {
				log.Println("closing the connection")
				cmdconn.Close()
				log.Println("waiting the command")
				cmd.Wait()
				log.Println("wait done")
				cmd, cmdconn, err = startSession(
					[]string{"crsh-server", "echo"},
					"localhost:5679")
				if err != nil {
					return err
				}
				enc = json.NewEncoder(cmdconn)
			} else {
				enc.Encode(msg)
			}
		}
	}

	return nil
}

func startSession(cmdline []string, addr string) (*exec.Cmd, net.Conn, error) {
	if len(cmdline) == 0 {
		return nil, nil, errors.New("cmdline cannot be empty")
	}
	cmd := exec.Command(cmdline[0], cmdline[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Start()
	if err != nil {
		return nil, nil, err
	}

	log.Println("Starting dialing shell")
	cmdconn, err := net.Dial("tcp", addr)
	for err != nil {
		time.Sleep(time.Second / 5)
		cmdconn, err = net.Dial("tcp", addr)
	}
	log.Println("Ending dialing shell")

	return cmd, cmdconn, nil
}
