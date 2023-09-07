package main;

import (
	"io"
	"os"
	"fmt"
	"log"
	"net"
	"bufio"
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

	commands := make(chan string)
	go Execute(commands)
	for {
		client, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		go Serve(client, commands)
	}
}

func Execute(commands chan string) {
	for command := range commands {
		fmt.Printf("RECEIVED: %s\n", command)
	}
}

func Serve(client net.Conn, commands chan string) {
	reader := bufio.NewReader(client)
	for {
		line, _, err := reader.ReadLine()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		commands <- string(line)
	}
}