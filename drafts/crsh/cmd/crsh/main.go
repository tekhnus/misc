package main

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"github.com/peterh/liner"
	"io"
	"log"
	"net"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

func main() {
	log.SetFlags(log.Ldate | log.Ltime | log.Lmsgprefix)

	logger, err := net.Dial("tcp", "localhost:5679")
	if err == nil {
		defer logger.Close()
		log.SetOutput(logger)
	} else {
		log.Println("While trying to connect to logserver:", err)
	}

	_, command := filepath.Split(os.Args[0])
	args := os.Args[1:]

	if len(args) > 0 {
		if args[0] == "prompt" {
			command = args[0]
			args = args[1:]
		}
	}
	switch command {
	case "crsh":
		err = crsh(args)
	case "prompt":
		err = prompt(args)
	default:
		log.Fatalln("unknown command", command)
	}
	if err != nil {
		log.Fatal(err)
	}
}

func crsh(args []string) error {
	log.SetPrefix(fmt.Sprintf("%18s ", "crsh"))

	fset := flag.NewFlagSet("crsh", flag.ExitOnError)
	name := fset.String("name", "", "initial session name")
	host := fset.String("host", "localhost", "initial session host")
	fset.Parse(args)

	if *name == "" {
		id := os.Getpid()
		*name = fmt.Sprintf("default%d", id)
	}
	sockname := "crsh-manager-" + *name
	sock := filepath.Join("/tmp", sockname)
	url := "unix://" + sock

	err := SimpleRun(`kitty @ goto-layout splits`)
	if err != nil {
		return err
	}
	err = SimpleRun(fmt.Sprintf(`kitty @ launch --self --title crsh-prompt --location hsplit --cwd current crsh prompt %s`, url))
	if err != nil {
		return err
	}
	err = SimpleRun(`kitty @ resize-window --self --axis vertical --increment +1000`)
	if err != nil {
		return err
	}
	err = SimpleRun(fmt.Sprintf(`crsh-server manager -name %s -host %s %s`, *name, *host, url))
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
	log.SetPrefix(fmt.Sprintf("%18s ", "prompt"))

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
	defer close(outp)

	lnr := liner.NewLiner()
	defer lnr.Close()
	for {
		fmt.Print("\033[H\033[2J")
		err := SimpleRun(`kitty @ focus-window`)
		if err != nil {
			log.Println("Kitty focus error", err)
		}
		line, err := lnr.Prompt("> ")
		if err != nil {
			log.Println("Liner error", err)
			return
		}
		err = SimpleRun(`kitty @ focus-window --match title:crsh-server`)
		if err != nil {
			log.Println("Kitty unfocus error", err)
		}
		outp <- line
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
