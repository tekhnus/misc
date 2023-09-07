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
	"mvdan.cc/sh/v3/syntax"
	"mvdan.cc/sh/v3/interp"
)

const Addr = "/tmp/crsh.sock"

func main() {
	fmt.Printf("Hello, world!\n")
	err := os.RemoveAll(Addr)
	if err != nil {
		log.Fatal(err)
	}

	listener, err := net.Listen("unix", Addr)
	if err != nil {
		log.Fatal(err)
	}
	defer listener.Close()

	runner, err := interp.New()
	if err != nil {
		log.Fatal(err)
	}

	for {
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
			fmt.Printf("RECEIVED: %s\n", command)
			source, err := syntax.NewParser().Parse(strings.NewReader(string(command)), "")
			if err != nil {
				log.Fatal(err)
			}
			err = runner.Run(context.TODO(), source)
			if err != nil {
				log.Fatal(err)
			}
			client.Write([]byte("0\n"))
		}
	}
}

