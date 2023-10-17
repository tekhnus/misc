package main

import (
	"encoding/json"
	"strings"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
	"os"
	"os/exec"
	"time"
	"flag"
	"net/url"
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
	fset := flag.NewFlagSet("echo", flag.ExitOnError)
	fset.Parse(args)
	if fset.NArg() < 1 {
		return errors.New("expected a url")
	}
	addr := fset.Arg(0)
	url, err := url.Parse(addr)
	if err != nil {
		return err
	}
	listener, err := net.Listen(url.Scheme, url.Host)
	if err != nil {
		return err
	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}
		exit, _ := echoLoop(conn)
		if exit {
			break
		}
	}
	return nil
}

func echoLoop(conn net.Conn) (bool, error) {
	defer conn.Close()
	dec := json.NewDecoder(conn)
	for {
		var msg map[string]string
		err := dec.Decode(&msg)
		if err != nil {
			return false, err
		}
		fmt.Printf("received: %s\n", msg)
		if msg["type"] == "exit" {
			return true, nil
		}
	}
}

func manager(args []string) error {
	listener, err := net.Listen("tcp", "localhost:5678")
	if err != nil {
		return err
	}
	defer listener.Close()

	client, err := listener.Accept()
	if err != nil {
		return err
	}
	defer client.Close()
	dec := json.NewDecoder(client)

	url := "tcp://localhost:5679"
	serverProcess, server, err := startSession(
		[]string{"crsh-server", "echo", url},
		url)
	if err != nil {
		return err
	}
	enc := json.NewEncoder(server)
	exited := false
	for !exited {
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
			parsedMsg := strings.Split(msg["cmd"], " ")
			if parsedMsg[0] == "\\open" {
				if len(parsedMsg) != 2 {
					log.Println(parsedMsg)
					return errors.New("Expected a single argument")
				}
				name := parsedMsg[1]
				log.Println("closing the connection")
				enc.Encode(map[string]string{"type": "exit",})
				server.Close()
				log.Println("waiting the command")
				serverProcess.Wait()
				log.Println("wait done")
				aurl := "tcp://localhost:5679"
				serverProcess, server, err = startSession(
					[]string{"abduco", "-A", name, "crsh-server", "echo", aurl},
					aurl)
				if err != nil {
					return err
				}
				fmt.Println("connected to new session")
				enc = json.NewEncoder(server)
			} else if parsedMsg[0] == "\\exit" {
				log.Println("closing the connection")
				enc.Encode(map[string]string{"type": "exit",})
				server.Close()
				log.Println("waiting the command")
				serverProcess.Wait()
				log.Println("wait done")
				exited = true
				break
			} else {
				enc.Encode(msg)
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

	log.Println("Starting dialing shell")
	cmdconn, err := net.Dial(url.Scheme, url.Host)
	for err != nil {
		log.Println(err)
		time.Sleep(time.Second / 5)
		cmdconn, err = net.Dial(url.Scheme, url.Host)
	}
	log.Println("Ending dialing shell")

	return cmd, cmdconn, nil
}
