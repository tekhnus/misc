package main;

import (
	"io"
	"os"
	"fmt"
	"log"
	"net"
	"bufio"
	"errors"
	"context"
	"strings"
	"golang.org/x/term"
	"mvdan.cc/sh/v3/syntax"
	"mvdan.cc/sh/v3/interp"
)

func main() {
	logfile, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		log.Println(err)
		return
	}
	defer logfile.Close()
	log.SetOutput(logfile)

	err = mainImpl()
	if err != nil {
		log.Println(err)
		return
	}
	log.Println("Exiting server normally")
}

func mainImpl() error {
	logfile, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		return (err)
	}
	defer logfile.Close()
	log.SetOutput(logfile)

	log.Printf("Starting server\n")
	if len(os.Args) <= 1 {
		return errors.New("Not enough args")
	}
	addr := os.Args[1]

	err = os.RemoveAll(addr)
	if err != nil {
		return (err)
	}

	listener, err := net.Listen("unix", addr)
	if err != nil {
		return (err)
	}
	defer listener.Close()

	log.Printf("Started listening\n")
	runner, err := interp.New(interp.StdIO(os.Stdin, os.Stdout, os.Stderr))
	if err != nil {
		return (err)
	}

	_, rows, err := term.GetSize(0)
	if err != nil {
		return (err)
	}
	fmt.Printf("\033[%d;0H", rows)
	exited := false
	for !exited {
		log.Printf("Waiting for connection\n")
		err = func() error {
			client, err := listener.Accept()
			if err != nil {
				return (err)
			}
			defer client.Close()
			log.Printf("Connection received\n")

			reader := bufio.NewReader(client)
			for {
				log.Printf("Reading a line\n")
				_command, _, err := reader.ReadLine()
				log.Printf("Done reading a line\n")
				if err == io.EOF {
					log.Printf("Received an EOF\n")
					break
				}
				if err != nil {
					log.Printf("Read error\n")
					return (err)
				}
				command := string(_command)
				log.Printf("Read success: %s\n", command)
				if strings.HasPrefix(command, "EVAL ") {
					log.Printf("Eval request\n")
					cmd := command[5:]
					fmt.Printf("> %s\n", cmd)
					source, err := syntax.NewParser().Parse(strings.NewReader(string(cmd)), "")
					if err != nil {
						return (err)
					}
					for _, stmt := range source.Stmts {
						err = runner.Run(context.TODO(), stmt)
						if err != nil {
							client.Write([]byte("1\n"))
						} else {
							client.Write([]byte("0\n"))
						}
						exited = runner.Exited()
						if exited {
							break
						}
					}
					fmt.Printf("---\n")
				} else if strings.HasPrefix(command, "COMPLETE ") {
					log.Printf("Complete request\n")
					fmt.Fprintf(client, "\n")
				} else if strings.HasPrefix(command, "HELLO") {
					log.Printf("Hello request\n")
					fmt.Fprintf(client, "HELLO\n")
					break
				} else {
					return errors.New("Unknown command")
				}
			}
			return nil
		}()
		log.Printf("Connection done\n")
		if err != nil {
			return err
		}
	}
	log.Printf("Exiting server\n");
	return nil
}

