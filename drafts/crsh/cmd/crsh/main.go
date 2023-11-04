package main

import (
	"log"
	"os"
)

func main() {
	cmd := "manager"
	args := os.Args[1:]

	if len(args) > 0 {
		switch args[0] {
		case "manager", "log-server":
			cmd = args[0]
			args = args[1:]
		}
	}

	var res error
	switch cmd {
	case "manager":
		res = Manager(args)
	case "log-server":
		res = LogServer(args)
	default:
		log.Fatal("Unknown command")
	}

	if res != nil {
		log.Fatal(res)
	}
}

func Manager(args []string) error {
	return nil
}

func LogServer(args []string) error {
	return nil
}
