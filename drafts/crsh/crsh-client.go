package main

import (
	"log"
	"fmt"
	"os"
	"os/exec"
	"bufio"
	"path/filepath"
	"strings"

	"github.com/peterh/liner"
)

var (
	history_fn = filepath.Join(os.TempDir(), ".liner_example_history")
	names      = []string{"john", "james", "mary", "nancy"}
)

func main() {
	line := liner.NewLiner()
	defer line.Close()

	chandir := os.Args[1]

	infilename := chandir + "/result"
	infile, err := os.Create(infilename)
	if err != nil {
		return
	}
	defer infile.Close()
	inreader := bufio.NewReader(infile)

	outfilename := chandir + "/command"
	outfile, err := os.Create(outfilename)
	if err != nil {
		return
	}
	defer outfile.Close()

	servername := os.Args[2]

	line.SetCtrlCAborts(true)

	line.SetCompleter(func(line string) (c []string) {
		for _, n := range names {
			if strings.HasPrefix(n, strings.ToLower(line)) {
				c = append(c, n)
			}
		}
		return
	})

	if f, err := os.Open(history_fn); err == nil {
		line.ReadHistory(f)
		f.Close()
	}

	// log.Print("Ready")
	for {
		fmt.Printf("\033[9999;1H\x1b[38;5;251m@%s\x1b[K\x1b[0m\033[1;0H", servername)
		response, isprefix, err := inreader.ReadLine()
		// log.Print("Response")
		if err != nil || isprefix {
			break
		}
		if len(response) == 0 {
			break
		}
		err = exec.Command("kitty", "@", "focus-window").Run()
		if err != nil {
			panic(err)
		}
		if name, err := line.Prompt("> "); err == nil {
			fmt.Fprintln(outfile, "EVAL", name)
			line.AppendHistory(name)
			if name == "exit" {
				break
			}
		} else if err == liner.ErrPromptAborted {
			log.Print("Aborted")
		} else {
			log.Print("Error reading line: ", err)
		}

		if f, err := os.Create(history_fn); err != nil {
			log.Print("Error writing history file: ", err)
		} else {
			line.WriteHistory(f)
			f.Close()
		}
		fmt.Print("\033[H\033[2J")
		err = exec.Command("kitty", "@", "focus-window", "-m", "cmdline:.*crsh-.*").Run()
		if err != nil {
			panic(err)
		}
	}
}