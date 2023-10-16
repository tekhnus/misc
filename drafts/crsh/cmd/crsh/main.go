package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net"
	"time"
)

func main() {
	err := client()
	if err != nil {
		log.Fatal(err)
	}
}

func client() error {
	conn, err := net.Dial("tcp", "localhost:5678")
	for err != nil {
		time.Sleep(time.Second / 5)
		conn, err = net.Dial("tcp", "localhost:5678")
	}
	defer conn.Close()

	enc := json.NewEncoder(conn)

	commands := make(chan string)
	go readPrompt(commands)

	for {
		select {
			case cmd := <- commands:
				msg := map[string]string{
					"cmd": cmd,
				}

				err = enc.Encode(msg)
				if err != nil {
					return err
				}
		}
	}

	return nil
}

func readPrompt(outp chan string) {
	for {
		var cmd string
		fmt.Scanln(&cmd)
		outp <- cmd
	}
}
