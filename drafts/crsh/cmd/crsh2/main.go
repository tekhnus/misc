package main

import (
	"os"
	"log"
	"net"
	"os/exec"
)

func main() {
	log.SetPrefix("crsh2 ")
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
	chn := "/tmp/crsh-foo-bar"
	name := "hi"

	cmd := exec.Command("/Users/zahaaar/.local/bin/crsh-server-in-ssh", chn, name)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Start()
	if err != nil {
		return err
	}
	defer cmd.Wait()

	cmdClient := exec.Command("/Users/zahaaar/.local/bin/crsh-client", chn, name)
	cmdClient.Stdin = os.Stdin
	cmdClient.Stdout = os.Stdout
	cmdClient.Stderr = os.Stderr
	err = cmdClient.Start()
	if err != nil {
		return err
	}
	defer cmdClient.Wait()

	return nil
}
