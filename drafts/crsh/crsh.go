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
			client.Write([]byte("0\n"))
		}
	}
}

