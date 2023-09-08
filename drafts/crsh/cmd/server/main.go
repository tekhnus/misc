package main;

import (
	"io"
	"os"
	"fmt"
	"log"
	"net"
	"bufio"
	"context"
	"strings"
	"golang.org/x/term"
	"mvdan.cc/sh/v3/syntax"
	"mvdan.cc/sh/v3/interp"
)

func main() {
	logfile, err := net.Dial("unix", "/tmp/crsh.log")
	if err != nil {
		log.Fatal(err)
	}
	defer logfile.Close()
	log.SetOutput(logfile)

	log.Printf("Starting server\n")
	if len(os.Args) <= 1 {
		log.Fatal("Not enough args")
	}
	addr := os.Args[1]

	err = os.RemoveAll(addr)
	if err != nil {
		log.Fatal(err)
	}

	listener, err := net.Listen("unix", addr)
	if err != nil {
		log.Fatal(err)
	}
	defer listener.Close()

	runner, err := interp.New(interp.StdIO(os.Stdin, os.Stdout, os.Stderr))
	if err != nil {
		log.Fatal(err)
	}

	_, rows, err := term.GetSize(0)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Printf("\033[%d;0H", rows)
	exited := false
	for !exited {
		client, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		reader := bufio.NewReader(client)
		for {
			_command, _, err := reader.ReadLine()
			command := string(_command)
			if err == io.EOF {
				break
			}
			if err != nil {
				log.Fatal(err)
			}
			if command[:4] == "EVAL" {
				cmd := command[5:]
				fmt.Printf("> %s\n", cmd)
				source, err := syntax.NewParser().Parse(strings.NewReader(string(cmd)), "")
				if err != nil {
					log.Fatal(err)
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
			}
		}
	}
	log.Printf("Exiting server\n");
}

