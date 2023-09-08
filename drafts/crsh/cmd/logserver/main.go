package main;

import (
	"io"
	"os"
	"fmt"
	"log"
	"net"
	"bufio"
)

const Addr = "/tmp/crsh.log"

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

	for {
		client, err := listener.Accept()
		if err != nil {
			log.Fatal(err)
		}
		go Serve(client)
	}
}

func Serve(client net.Conn) {
	defer client.Close()
	reader := bufio.NewReader(client)
	for {
		line, _, err := reader.ReadLine()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatal(err)
		}
		fmt.Printf("%s\n", line)
	}
}