package main

import (
	"log"
	"fmt"
	"os"
	"io"
	"net"
	"time"
	"bufio"
	"errors"
	"os/exec"
	"path/filepath"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".liner_example_history")
)

func main() {
	log.SetPrefix("client ")
	logfile, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		log.Println(err)
	} else {
		defer logfile.Close()
		log.SetOutput(logfile)
	}

	err = mainImpl()
	if err != nil {
		log.Println(err)
		return
	}
}

func mainImpl() error {
	log.Printf("Starting client\n")
	if len(os.Args) <= 1 {
		return errors.New("Not enough args")
	}
	addr := os.Args[1]

	log.Printf("Saying hello\n")
	server, err := dialAndCheck(addr)
	for err != nil {
		log.Println(err)
		time.Sleep(time.Second / 2)
		log.Printf("Retrying hello\n")
		server, err = dialAndCheck(addr)
	}
	defer server.Close()
	log.Printf("Said hello\n")
	reader := bufio.NewReader(server)

	fmt.Print("\x1b[?1049h")
	defer fmt.Print("\x1b[?1049l")

	line := liner.NewLiner()
	defer line.Close()

	line.SetCtrlCAborts(true)

	line.SetTabCompletionStyle(liner.TabPrints)
	line.SetCompleter(func(line string) (c []string) {
		fmt.Fprintf(server, "COMPLETE %s\n", line)
		for {
			response, isprefix, err := reader.ReadLine()
			if isprefix || err != nil {
				panic("bad")
			}
			if len(response) == 0 {
				break
			}
			c = append(c, string(response))
		}
		return
	})

	if f, err := os.Open(history_fn); err == nil {
		line.ReadHistory(f)
		f.Close()
	}

	// log.Print("Ready")
	for {
		fmt.Printf("\033[9999;1H\x1b[38;5;251m@%s\x1b[K\x1b[0m\033[1;0H", "servername")
		// if err != nil {
		// 	panic(err)
		// }
		err = exec.Command("kitty", "@", "focus-window").Run()
		if name, err := line.Prompt("> "); err == nil {
			err = exec.Command("kitty", "@", "focus-window", "-m", "title:.*crsh-server.*").Run()
			fmt.Fprintf(server, "EVAL %s\n", name)
			line.AppendHistory(name)
			if name == "exit" {
				break
			}
			if f, err := os.Create(history_fn); err != nil {
				log.Print("Error writing history file: ", err)
			} else {
				line.WriteHistory(f)
				f.Close()
			}
			fmt.Print("\033[H\033[2J")
			// if err != nil {
			// 	panic(err)
			// }
			response, isprefix, err := reader.ReadLine()
			if err == io.EOF {
				return errors.New("Received a sudden EOF from server")
			}
			if err != nil {
				return (err)
			}
			if isprefix {
				return errors.New("Incomplete read")
			}
			if len(response) == 0 {
				break
			}
		} else if err == liner.ErrPromptAborted {
			fmt.Print("\033[H\033[2J")
		} else if err == io.EOF {
			break
		} else {
			log.Print("Error reading line: ", err)
		}

	}
	log.Printf("Exiting client\n");
	return nil
}

func dialAndCheck(addr string) (net.Conn, error) {
	server, err := net.Dial("unix", addr)
	if err != nil {
		return nil, err
	}
	fmt.Fprintf(server, "HELLO\n")
	var resp [6]byte
	log.Printf("Reading hello\n")
	_, err = io.ReadFull(server, resp[:])
	if err != nil {
		server.Close()
		return nil, err
	}
	log.Printf("Recieved a potential hello\n")
	if string(resp[:]) != "HELLO\n" {
		server.Close()
		return nil, errors.New("Didn't receive a proper hello")
	}
	return server, nil
}