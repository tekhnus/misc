package main;

import (
	"io"
	"fmt"
	"log"
	"net"
	"bufio"
)

func main() {
	listener, err := net.Listen("tcp", "localhost:5678")
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