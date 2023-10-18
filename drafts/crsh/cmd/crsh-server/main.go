package main

import (
	"encoding/json"
	"path/filepath"
	"context"
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
	"bufio"
	"os/signal"
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
	case "logserver":
		err = logserver()
	default:
		log.Fatal("Unknown command")
	}

	if err != nil {
		log.Fatal(err)
	}
}

func echo(args []string, ctx context.Context) error {
	log.SetPrefix("echo ")

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
	listener, err := net.Listen(url.Scheme, url.Host + url.Path)
	if err != nil {
		return err
	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}
		exit, _ := echoLoop(conn, ctx)
		if exit {
			break
		}
	}
	return nil
}

func echoLoop(conn net.Conn, ctx context.Context) (bool, error) {
	defer conn.Close()
	msgs := make(chan map[string]string)
	go readMessages(conn, msgs)
	for {
		select {
		case msg := <- msgs:
		fmt.Printf("received: %s\n", msg)
		if msg["type"] == "exit" {
			return true, nil
		}
		case <- ctx.Done():
		return true, nil
		}
	}
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
	log.SetPrefix("manager ")

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
	listener, err := net.Listen(murl.Scheme, murl.Host + murl.Path)
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

	sock := filepath.Join(os.TempDir(), "default")
	url := "unix://" + sock
	serverProcess, server, err := startSession(
		[]string{"crsh-server", "echo", url},
		url)
	if err != nil {
		return err
	}
	defer server.Close()
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
				server.Close()
				log.Println("killing the server")
				serverProcess.Process.Signal(os.Interrupt)
				serverProcess.Wait()
				log.Println("wait done")
				asock := filepath.Join(os.TempDir(), name)
				aurl := "unix://" + asock
				serverProcess, server, err = startSession(
					[]string{"abduco", "-A", name, "crsh-server", "echo", aurl},
					aurl)
				if err != nil {
					return err
				}
				fmt.Println("connected to new session")
				enc = json.NewEncoder(server)
			} else if parsedMsg[0] == "\\exit" {
				log.Println("exiting the server")
				enc.Encode(map[string]string{"type": "exit",})
				log.Println("closing the connection")
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

	log.Println("Starting dialing shell", addr, url.Scheme, url.Host + url.Path)
	cmdconn, err := net.Dial(url.Scheme, url.Host + url.Path)
	for err != nil {
		log.Println(err)
		time.Sleep(time.Second / 5)
		cmdconn, err = net.Dial(url.Scheme, url.Host + url.Path)
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
