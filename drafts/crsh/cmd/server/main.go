package main;

import (
	"io"
	"os"
	"log"
	"net"
	"bufio"
	"context"
	"strings"
	"mvdan.cc/sh/v3/syntax"
	"mvdan.cc/sh/v3/interp"
)

const Addr = "/tmp/crsh.sock"

func main() {
	err := os.RemoveAll(Addr)
	if err != nil {
		log.Fatal(err)
	}

	listener, err := net.Listen("unix", Addr)
	if err != nil {
		log.Fatal(err)
	}
	defer listener.Close()

	runner, err := interp.New(interp.StdIO(os.Stdin, os.Stdout, os.Stderr))
	if err != nil {
		log.Fatal(err)
	}

	exited := false
	for !exited {
		client, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		reader := bufio.NewReader(client)
		for {
			command, _, err := reader.ReadLine()
			if err == io.EOF {
				break
			}
			if err != nil {
				log.Fatal(err)
			}
			source, err := syntax.NewParser().Parse(strings.NewReader(string(command)), "")
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
		}
	}
}

