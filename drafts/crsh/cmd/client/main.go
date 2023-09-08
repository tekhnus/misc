package main

import (
	"log"
	"fmt"
	"os"
	"io"
	"net"
	"bufio"
	"os/exec"
	"path/filepath"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".liner_example_history")
)

func main() {
	logfile, err := net.Dial("unix", "/tmp/crsh.log")
	if err != nil {
		log.Fatal(err)
	}
	defer logfile.Close()
	log.SetOutput(logfile)

	log.Printf("Starting client\n")
	if len(os.Args) <= 1 {
		log.Fatal("Not enough args")
	}
	addr := os.Args[1]

	server, err := net.Dial("unix", addr)
	if err != nil {
		log.Fatal(err)
	}
	defer server.Close()
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
			if err != nil || isprefix {
				break
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
}