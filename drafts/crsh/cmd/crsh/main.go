package main

import (
	"bufio"
	"fmt"
	"encoding/json"
	"errors"
	"flag"
	"io"
	"log"
	"net"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"time"
	"strings"
)

func main() {
	_, command := filepath.Split(os.Args[0])
	args := os.Args[1:]

	if len(args) > 0 {
		if args[0] == "prompt" {
			command = args[0]
			args = args[1:]
		}
	}
	var err error
	switch command {
	case "crsh":
		err = crsh(args)
	case "prompt":
		err = prompt(args)
	default:
		time.Sleep(1)
		log.Fatalln("unknown command", command)
	}
	if err != nil {
		time.Sleep(1)
		log.Fatal(err)
	}
}

func crsh(args []string) error {
	fset := flag.NewFlagSet("crsh", flag.ExitOnError)
	fset.Parse(args)

	id := os.Getpid()
	name := fmt.Sprintf("crsh%d", id)
	sockname := "crsh-socket-" + name
	sock := filepath.Join("/tmp", sockname)
	url := "unix://" + sock

	err := SimpleRun(`kitty @ goto-layout splits`)
	if err != nil {
		return err
	}
	err = SimpleRun(fmt.Sprintf(`kitty @ launch --self --location hsplit --cwd current crsh prompt %s`, url))
	if err != nil {
		return err
	}
	err = SimpleRun(`kitty @ resize-window --self --axis vertical --increment +1000`)
	if err != nil {
		return err
	}
	err = SimpleRun(fmt.Sprintf(`crsh-server manager -name default%d %s`, id, url))
	if err != nil {
		return err
	}
	return nil
}

func SimpleRun(comm string) error {
	args := strings.Split(comm, " ")
	cmd := exec.Command(args[0], args[1:]...)

	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	return cmd.Run()
}

func prompt(args []string) error {
	fset := flag.NewFlagSet("prompt", flag.ExitOnError)
	fset.Parse(args)
	if fset.NArg() < 1 {
		return errors.New("expected a url")
	}
	addr := fset.Arg(0)
	murl, err := url.Parse(addr)
	if err != nil {
		return err
	}

	conn, err := net.Dial(murl.Scheme, murl.Host+murl.Path)
	for err != nil {
		time.Sleep(time.Second / 5)
		conn, err = net.Dial(murl.Scheme, murl.Host+murl.Path)
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
		case cmd, ok := <-commands:
			if !ok {
				log.Println("Received an EOF from reader, sending exit")
				commands = nil
				cmd = "exit"
			}
			msg := map[string]string{
				"type": "cmd",
				"cmd":  cmd,
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
	log.Println("Received EOF while reading prompt")
	close(outp)
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
