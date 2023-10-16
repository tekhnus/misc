package main

import (
	"encoding/json"
	"errors"
	"bufio"
	"os"
	"io"
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

	replies := make(chan map[string]string)
	go readMessages(conn, replies)

Loop:
	for {
		select {
		case cmd := <-commands:
			msg := map[string]string{
				"cmd": cmd,
			}

			err = enc.Encode(msg)
			if err != nil {
				return err
			}
		case msg := <-replies:
			log.Println("Received a message")
			msgtype := msg["type"]
			switch msgtype {
			case "end":
				log.Println("Received an end message")
				break Loop
			default:
				return errors.New("Unknown message type")
			}
		}
	}

	log.Println("Exiting")
	return nil
}

func readPrompt(outp chan string) {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		outp <- scanner.Text()
	}
}

func readMessages(conn net.Conn, outp chan map[string]string) {
	dec := json.NewDecoder(conn)
	for {
		var msg map[string]string
		err := dec.Decode(&msg)
		if err != nil {
			var isEof string
			if err == io.EOF {
				isEof = "1"
			} else {
				isEof = "0"
			}
			outp <- map[string]string{"type": "end", "is_eof": isEof, "error": err.Error()}
			break
		}
		outp <- msg
	}
}
