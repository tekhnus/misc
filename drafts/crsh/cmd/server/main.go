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
		err = func() error {
			client, err := listener.Accept()
			if err != nil {
				return (err)
			}
			defer client.Close()

			reader := bufio.NewReader(client)
			for {
				_command, _, err := reader.ReadLine()
				command := string(_command)
				if err == io.EOF {
					break
				}
				if err != nil {
					return (err)
				}
				if command[:4] == "EVAL" {
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
				} else if command[:8] == "COMPLETE" {
					fmt.Fprintf(client, "\n")
				} else if command == "HELLO" {
					fmt.Fprintf(client, "HELLO\n")
					break
				}
			}
			return nil
		}()
		if err != nil {
			return err
		}
	}
	log.Printf("Exiting server\n");
	return nil
}

