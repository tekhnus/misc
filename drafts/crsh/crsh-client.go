package main

import (
	"log"
	"fmt"
	"os"
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

	outfilename := os.Args[1]
	outfile, err := os.Create(outfilename)
	if err != nil {
		return
	}
	defer outfile.Close()

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

	if name, err := line.Prompt("> "); err == nil {
		fmt.Fprintln(outfile, "EVAL", name)
		line.AppendHistory(name)
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
}