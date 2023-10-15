package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net"
	"os"
)

func main() {
	if len(os.Args) <= 1 {
		log.Fatal("Not enough arguments")
	}

	cmd := os.Args[1]
	args := os.Args[2:]

	var err error

	switch cmd {
	case "echo":
		err = echo(args)
	default:
		log.Fatal("Unknown command")
	}

	if err != nil {
		log.Fatal(err)
	}
}

func echo(args []string) error {
	listener, err := net.Listen("tcp", "localhost:5678")
	if err != nil {
		return err
	}
	defer listener.Close()

	conn, err := listener.Accept()
	if err != nil {
		return err
	}
	defer conn.Close()

	dec := json.NewDecoder(conn)
	for {
		var msg map[string]string
		err = dec.Decode(&msg)
		if err != nil {
			return err
		}
		fmt.Printf("received: %s\n", msg)
	}
	return nil
}
