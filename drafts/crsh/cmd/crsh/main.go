package main

import (
	"bufio"
	"context"
	_ "embed"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"math/rand"
	"net"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/peterh/liner"
	"mvdan.cc/sh/v3/interp"
	"mvdan.cc/sh/v3/syntax"
)

//go:embed git-head.txt
var Version string
var Executable string = os.Args[0]

func main() {
	err := Main()
	if err != nil {
		os.Exit(1)
	}
	os.Exit(0)
}

func Main() error {
	log.SetFlags(log.Ldate | log.Ltime | log.Lshortfile | log.Lmsgprefix)

	logServer, err := net.Dial("tcp", "localhost:5678")
	if err != nil {
		log.SetOutput(io.Discard)
	} else {
		defer logServer.Close()
		log.SetOutput(logServer)
	}

	signals := make(chan os.Signal, 16)
	signal.Notify(signals, os.Interrupt, syscall.SIGHUP)

	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		signal := <-signals
		log.Println("Received a signal", signal)
		cancel()
	}()

	cmdline := strings.Join(os.Args, " ")
	log.SetPrefix(fmt.Sprintf("%18s: ", cmdline))

	log.Println("Process started:", Executable)

	var cmd string
	var args []string

	if len(os.Args) > 1 {
		cmd = os.Args[1]
		args = os.Args[2:]
	} else {
		cmd = "manager"
	}

	switch cmd {
	case "manager":
		err = ManagerMain(args, ctx)
	case "shell":
		err = ShellMain(args, ctx)
	case "ssh":
		err = SSHMain(args, ctx)
	case "log-server":
		err = LogServerMain(args, ctx)
	default:
		err = fmt.Errorf("Unknown command: %s", cmd)
	}

	if err != nil {
		log.Println(err)
	}

	return err
}

func ManagerMain(args []string, ctx context.Context) error {
	host := "^"
	name := RandomName()

	// Doing it in the beginning because it messes with
	// terminal modes which better done before tmux runs.
	lnr := liner.NewLiner()
	lnr.Close()

	for {
		fmt.Println("Connecting to session", SessionName(host, name), "...")
		shell, err := MakeShell(host, name)
		if err != nil {
			return err
		}
		newhost, newname, exit, err := HandleShell(shell, lnr, ctx)
		if err != nil {
			return err
		}
		if exit {
			break
		}
		if newname != "" {
			name = newname
		}
		if newhost != "" {
			host = newhost
		}
	}

	log.Println("Exiting")
	return nil
}

func HandleShell(shell Shell, lnr *liner.State, ctx context.Context) (string, string, bool, error) {
	defer func() {
		log.Println("Start waiting on shell termination")
		for shell.Out != nil || shell.Done != nil {
			select {
			case _, ok := <-shell.Out:
				if !ok {
					shell.Out = nil
				}
			case _, ok := <-shell.Done:
				if !ok {
					shell.Done = nil
				}
			}
		}
		log.Println("Finish waiting on shell termination")
	}()
	defer shell.Detach()

	err := SyncHistoryAndAppend(lnr, "")
	if err != nil {
		log.Println(err)
	}
	var hst strings.Builder
	_, err = lnr.WriteHistory(&hst)
	if err != nil {
		log.Println(err)
	}
	log.Println("Start sending history")
	shell.In.Encode(Message{Type: "history", Payload: hst.String()})
	log.Println("Finish sending history")

	for shell.Out != nil || shell.Done != nil {
		log.Println("Selecting...")
		select {
		case msg, ok := <-shell.Out:
			if !ok {
				log.Println("Shell output was closed")
				shell.Out = nil
				log.Println("Start detaching")
				return "", "", true, nil
			}
			log.Println("Received a message from shell")
			switch msg.Type {
			case "input":
				err := SyncHistoryAndAppend(lnr, msg.Payload)
				if err != nil {
					log.Println(err)
				}
				if strings.HasPrefix(msg.Payload, "\\") {
					tokens := strings.Split(msg.Payload, " ")
					switch tokens[0] {
					case "\\go":
						if len(tokens) != 2 {
							shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
							break
						}
						log.Println("Sending an exit message to current shell")
						shell.In.Encode(Message{Type: "execute", Payload: "exit"})
						log.Println("Stopping the shell")
						name := tokens[1]
						return "", name, false, nil
					case "\\ssh":
						if len(tokens) != 2 {
							shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
							break
						}
						log.Println("Sending an exit message to current shell")
						shell.In.Encode(Message{Type: "execute", Payload: "exit"})
						log.Println("Stopping the shell")
						host := tokens[1]
						return host, "", false, nil
					case "\\detach":
						if len(tokens) != 1 {
							shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
							break
						}
						return "", "", true, nil
					default:
						shell.In.Encode(Message{Type: "execute", Payload: "echo Wrong command"})
					}
				} else {
					shell.In.Encode(Message{Type: "execute", Payload: msg.Payload})
				}
			default:
				return "", "", false, fmt.Errorf("Unknown message type: %s", msg.Type)
			}
		case err, ok := <-shell.Done:
			if !ok {
				log.Println("Shell status channel was closed")
				shell.Done = nil
				break
			}
			log.Println("Shell command finished")
			if err != nil {
				log.Println("With error:", err)
				return "", "", true, err
			}
		case <-ctx.Done():
			log.Println("Start terminating by cancel")
			return "", "", true, nil
		}
	}

	log.Println("All channels closed")
	return "", "", false, fmt.Errorf("All channels were closed")
}

func SyncHistoryAndAppend(lnr *liner.State, entry string) error {
	// TODO: interprocess locking should be done here.
	histfile := os.ExpandEnv("$HOME/.crsh-history")
	lnr.ClearHistory()
	hist, err := os.Open(histfile)
	if err != nil {
		log.Println(err)
	} else {
		defer hist.Close()
		_, err = lnr.ReadHistory(hist)
		if err != nil {
			return err
		}
	}
	if entry != "" {
		lnr.AppendHistory(entry)
	}
	history, err := os.Create(histfile)
	if err != nil {
		return err
	}
	defer history.Close()
	_, err = lnr.WriteHistory(history)
	return err
}

type Shell = struct {
	In     *json.Encoder
	Out    chan Message
	Done   chan error
	Detach func() error
}

func MakeShell(host string, name string) (Shell, error) {
	shellCtx, cancel := context.WithCancel(context.Background())

	var shellIn *json.Encoder
	shellOut := make(chan Message)
	shellCmdOut := make(chan error)

	log.Println("Start launching shell", host, name)
	shellCmd := MakeShellCommand(host, name)
	go func() {
		log.Println("Start running shell:", shellCmd)
		err := shellCmd.Run()
		log.Println("Finish running shell:", err)
		cancel()
		shellCmdOut <- err
		close(shellCmdOut)
	}()
	log.Println("Finish launching shell")

	log.Println("Start dialing shell")
	shell, err := MakeShellConnection(name, shellCtx)
	if err != nil {
		log.Println("While dialing shell:", err)
		log.Println("Waiting for shell command to complete")
		for range shellCmdOut {
		}
		return Shell{}, err
	}
	shellIn = json.NewEncoder(shell)
	go func() {
		ReadJsons(shell, shellOut)
		shell.Close()
		close(shellOut)
	}()
	log.Println("Finish dialing shell")

	detach := func() error {
		err := shell.Close()
		if err != nil {
			log.Println(err)
		}
		detachCmd := MakeDetachCommand(host, name)
		err = detachCmd.Run()
		if err != nil {
			log.Println(err)
		}
		return nil
	}
	return Shell{shellIn, shellOut, shellCmdOut, detach}, nil
}

func MakeShellCommand(host string, name string) *exec.Cmd {
	shellCmd := exec.Command(Executable, "ssh", host, name)

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	return shellCmd
}

func MakeDetachCommand(host string, name string) *exec.Cmd {
	args := []string{}
	if host != "^" {
		args = append(args, "ssh", host)
	}
	args = append(args, "tmux", "-L", "crsh-tmux", "detach-client", "-s", "="+SessionName(host, name))
	shellCmd := exec.Command(args[0], args[1:]...)

	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	return shellCmd
}

func MakeShellConnection(name string, ctx context.Context) (net.Conn, error) {
	for {
		select {
		case <-ctx.Done():
			return nil, fmt.Errorf("Cancelled")
		default:
		}
		shell, err := net.Dial("unix", GetSocketPath(name))
		if err != nil {
			log.Println(err)
			time.Sleep(time.Second / 5)
			continue
		}
		return shell, nil
	}
}

func ShellMain(args []string, ctx context.Context) error {
	fs := flag.NewFlagSet("shell", flag.ContinueOnError)
	err := fs.Parse(args)
	if err != nil {
		return err
	}
	if fs.NArg() != 1 {
		return fmt.Errorf("Expected one argument")
	}
	name := fs.Arg(0)

	listener, err := net.Listen("unix", GetSocketPath(name))
	if err != nil {
		return err
	}
	defer listener.Close()

	sessions, err := ListSessions()
	if err != nil {
		return err
	}

	header := false
	for _, session := range sessions {
		if session == name {
			continue
		}
		if !header {
			fmt.Println("Sessions on this host:")
			header = true
		}
		fmt.Printf("- %s\n", session)
	}

	runner, err := interp.New(interp.StdIO(os.Stdin, os.Stdout, os.Stderr))
	if err != nil {
		return err
	}

	normalMode, err := liner.TerminalMode()
	if err != nil {
		return err
	}
	lnr := liner.NewLiner()
	linerMode, err := liner.TerminalMode()
	if err != nil {
		return err
	}
	defer lnr.Close()
	normalMode.ApplyMode()

	lnr.SetTabCompletionStyle(liner.TabPrints)
	lnr.SetCompleter(Complete)

	state := State{runner: runner, lnr: lnr, defaultMode: normalMode, linerMode: linerMode}
	managers := make(chan net.Conn)
	go func() {
		err := Accept(listener, managers)
		if err != nil {
			log.Println(err)
		}
		close(managers)
	}()
	log.Println("Start accepting connection")
	for {
		select {
		case manager, ok := <-managers:
			if !ok {
				log.Println("Finish listening")
				return nil
			}
			log.Println("Finish accepting connection")
			cont, err := HandleManager(state, manager, ctx)
			if err != nil {
				return err
			}
			if !cont {
				return nil
			}
			log.Println("Start accepting connection")
		case <-ctx.Done():
			log.Println("Cancelled")
			return nil
		}
	}
}

func GetSocketPath(name string) string {
	return "/tmp/crsh-shell-" + name
}

func HandleManager(state State, manager net.Conn, ctx context.Context) (bool, error) {
	defer manager.Close()

	managerIn := json.NewEncoder(manager)

	managerOut := make(chan Message)
	go func() {
		ReadJsons(manager, managerOut)
		close(managerOut)
	}()

	log.Println("Starting history wait loop")
	select {
	case msg, ok := <-managerOut:
		if !ok {
			log.Println("No more messages from manager")
			return true, nil
		}
		pl := msg.Payload
		if len(pl) > 64 {
			pl = pl[:64] + "..."
		}
		log.Printf("Received: %s %#v\n", msg.Type, pl)
		switch msg.Type {
		case "history":
			log.Println("Received history")
			history := strings.NewReader(msg.Payload)
			state.lnr.ClearHistory()
			state.lnr.ReadHistory(history)
		default:
			return false, fmt.Errorf("Unknown message type: %s", msg.Type)
		}
	case <-ctx.Done():
		return false, nil
	}
	log.Println("Finish history wait loop")

	inputs := make(chan string)
	for {
		go func() {
			line, err := Prompt(state)
			if err != nil {
				log.Println(err)
				close(inputs)
				return
			}
			log.Printf("Sending line: %#v\n", line)
			inputs <- line
		}()

		log.Println("Starting input loop")
		select {
		case input, ok := <-inputs:
			if !ok {
				log.Println("No more input")
				return false, nil
			}
			log.Println("Start sending input to manager", input)
			err := managerIn.Encode(Message{Type: "input", Payload: input})
			log.Println("Finish sending input to manager", input)
			if err != nil {
				log.Println(err)
				return false, err
			}
		case <-ctx.Done():
			return false, nil
		}
		log.Println("Finishing input loop")

		select {
		case msg, ok := <-managerOut:
			if !ok {
				log.Println("No more messages from manager")
				return true, nil
			}
			pl := msg.Payload
			if len(pl) > 64 {
				pl = pl[:64] + "..."
			}
			log.Printf("Received: %s %#v\n", msg.Type, pl)
			switch msg.Type {
			case "execute":
				exit, err := SimpleExecute(state.runner, msg.Payload)
				if err != nil {
					fmt.Fprintln(os.Stderr, err)
				}
				if exit {
					log.Println("Exiting")
					return false, nil
				}
			default:
				return false, fmt.Errorf("Unknown message type: %s", msg.Type)
			}
		case <-ctx.Done():
			return false, nil
		}
	}
}

func Complete(prefix string) []string {
	var result []string
	words := strings.Split(prefix, " ")
	if len(words) == 1 {
		result = append(result, CompleteExecutable(words[0])...)
	} else {
		prevwords := strings.Join(words[:len(words)-1], " ") + " "
		lastword := words[len(words)-1]
		filecomps := CompleteFile(lastword)
		var fullcomps []string
		for _, cm := range filecomps {
			fullcomps = append(fullcomps, prevwords+cm)
		}
		result = append(result, fullcomps...)
	}
	return result
}

func CompleteExecutable(prefix string) []string {
	var result []string
	path := os.Getenv("PATH")
	for _, dir := range filepath.SplitList(path) {
		if dir == "" {
			dir = "."
		}
		names, err := filepath.Glob(dir + "/*")
		if err != nil {
			log.Println(err)
			continue
		}
		for _, fullname := range names {
			name := filepath.Base(fullname)
			if strings.HasPrefix(name, prefix) {
				result = append(result, name)
			}
		}
	}
	return result
}

func CompleteFile(prefix string) []string {
	names, err := filepath.Glob(prefix + "*")
	if err != nil {
		log.Println(err)
		return nil
	}
	return names
}

func SSHMain(args []string, ctx context.Context) error {
	var wg sync.WaitGroup
	defer func() {
		log.Println("Started waiting on child processes")
		wg.Wait()
		log.Println("Finished waiting on child processes")
	}()

	fs := flag.NewFlagSet("shell", flag.ContinueOnError)
	displayHost := fs.String("display-host", "", "host to display")
	err := fs.Parse(args)
	if err != nil {
		return err
	}
	if fs.NArg() != 2 {
		return fmt.Errorf("Expected two arguments")
	}
	host := fs.Arg(0)
	name := fs.Arg(1)
	if *displayHost == "" {
		*displayHost = host
	}

	if host == "^" {
		tmuxConf := os.ExpandEnv("$HOME/.local/share/crsh/" + Version + "/universal/tmux.conf")
		// TODO: suppress tmux's auxiliary output at detach.
		shellCmd := exec.Command("tmux", "-L", "crsh-tmux",
			"-f", tmuxConf, "new-session", "-A", "-s", SessionName(*displayHost, name),
			"env", "-u", "TMUX", Executable, "shell", name)
		shellCmd.Stdin = os.Stdin
		shellCmd.Stdout = os.Stdout
		shellCmd.Stderr = os.Stderr
		// Do not listening for signals here. If SIGINT or SIGHUP happens,
		// tmux will receive it too and exit.
		err = shellCmd.Run()
		log.Println("Multiplexer finished, exiting")
		return err
	}

	masterSocket := fmt.Sprintf("/tmp/crsh-ssh-%d", os.Getpid())

	shellSocket := GetSocketPath(name)
	masterCmd := exec.Command("ssh",
		"-M", "-S", masterSocket, "-N",
		host)
	wg.Add(1)
	go func() {
		defer wg.Done()
		log.Println("Started command:", masterCmd)
		out, err := masterCmd.CombinedOutput()
		log.Println("Finished command:", masterCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	}()

	masterExitCmd := exec.Command("ssh", "-S", masterSocket, "-O", "exit", host)
	defer func() {
		log.Println("Started command:", masterExitCmd)
		out, err := masterExitCmd.CombinedOutput()
		log.Println("Finished command:", masterExitCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	}()

	// I can't get dynamic master forwarding to work,
	// so not using master here.
	fwdCmd := exec.Command("ssh",
		"-L", shellSocket+":"+shellSocket,
		"-N",
		host)
	wg.Add(1)
	go func() {
		defer wg.Done()
		// FIXME Done so than the master appears
		time.Sleep(time.Second * 2)
		for {
			pingCmd := exec.Command(
				"ssh", "-S", masterSocket,
				"-O", "check",
				host)
			out, err := pingCmd.CombinedOutput()
			log.Println("Finished command:", pingCmd)
			log.Print("Output: ", string(out))
			log.Println("Status:", err)
			if err != nil {
				break
			}
			checkCmd := exec.Command(
				"ssh", "-S", masterSocket, host,
				"[", "-e", shellSocket, "]")
			out, err = checkCmd.CombinedOutput()
			log.Println("Finished command:", checkCmd)
			log.Print("Output: ", string(out))
			log.Println("Status:", err)
			if err == nil {
				break
			}
			time.Sleep(time.Second / 5)
		}
		defer os.Remove(shellSocket)
		log.Println("Started command:", fwdCmd)
		out, err := fwdCmd.CombinedOutput()
		log.Println("Finished command:", fwdCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	}()
	defer func() {
		proc := fwdCmd.Process
		if proc != nil {
			proc.Kill()
		}
	}()

	srcDir := os.ExpandEnv("$HOME/.local/share/crsh/" + Version)
	tmpDir := ".local/share/crsh/tmp"
	downloadDir := ".local/share/crsh/tmp/" + Version
	dstDir := ".local/share/crsh/" + Version

	testCmd := exec.Command(
		"ssh", "-S", masterSocket, host,
		"[", "-e", dstDir, "]")
	out, err := testCmd.CombinedOutput()
	log.Println("Finished command:", testCmd)
	log.Print("Output: ", string(out))
	log.Println("Status:", err)

	if err != nil {
		mkdirCmd := exec.Command(
			"ssh", "-S", masterSocket, host,
			"rm", "-rf", tmpDir,
			"&&", "mkdir", "-p", tmpDir)
		out, err := mkdirCmd.CombinedOutput()
		log.Println("Finished command:", mkdirCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)

		scpCmd := exec.Command(
			"scp", "-o", "ControlPath="+masterSocket,
			"-r", srcDir, host+":"+downloadDir)
		out, err = scpCmd.CombinedOutput()
		log.Println("Finished command:", scpCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)

		cpCmd := exec.Command(
			"ssh", "-S", masterSocket, host,
			"mv", downloadDir, dstDir)
		out, err = cpCmd.CombinedOutput()
		log.Println("Finished command:", cpCmd)
		log.Print("Output: ", string(out))
		log.Println("Status:", err)
	} else {
		log.Println("Skipping transfer")
	}

	executable := dstDir + "/linux/crsh"
	// TODO: suppress ssh's auxiliary output on closing.
	shellCmd := exec.Command("ssh",
		"-S", masterSocket, "-t", host,
		executable, "ssh", "-display-host", host, "^", name)
	shellCmd.Stdin = os.Stdin
	shellCmd.Stdout = os.Stdout
	shellCmd.Stderr = os.Stderr

	// Not processing signals here.
	// If SIGINT or SIGHUP happens,
	// the ssh should exit.
	err = shellCmd.Run()
	log.Println("Finished command:", shellCmd)
	log.Println("Status:", err)

	// FIXME I don't remember why this is needed
	time.Sleep(1 * time.Second)

	return nil
}

func ListSessions() ([]string, error) {
	var sessions []string
	sockets, err := filepath.Glob("/tmp/crsh-shell-*")
	if err != nil {
		return nil, err
	}
	for _, sock := range sockets {
		name, ok := strings.CutPrefix(sock, "/tmp/crsh-shell-")
		if !ok {
			return nil, fmt.Errorf("Unexpected problem")
		}
		sessions = append(sessions, name)
	}
	return sessions, nil
}

func SessionName(host string, name string) string {
	return name + "@" + host
}

func SimpleExecute(runner *interp.Runner, stmts string) (bool, error) {
	source, perr := syntax.NewParser().Parse(strings.NewReader(stmts), "")
	if perr != nil {
		return false, perr
	}
	var err error
	for _, stmt := range source.Stmts {
		serr := runner.Run(context.TODO(), stmt)
		if serr != nil {
			err = errors.Join(err, serr)
		}
		cherr := os.Chdir(runner.Dir)
		if cherr != nil {
			err = errors.Join(err, cherr)
		}
		if runner.Exited() {
			return true, nil
		}
	}
	return false, err
}

func Prompt(state State) (string, error) {
	cwd := state.runner.Dir
	home, _ := os.UserHomeDir()
	relcwd, _ := filepath.Rel(home, cwd)
	if !strings.HasPrefix(relcwd, "..") {
		cwd = "~/" + relcwd
		cwd = filepath.Clean(cwd)
	}
	fmt.Printf("\033[1m%s\033[0m\n", cwd)
	err := state.linerMode.ApplyMode()
	if err != nil {
		return "", err
	}
	line, err := state.lnr.Prompt("$ ")
	if err != nil {
		return "", err
	}
	state.lnr.AppendHistory(line)
	err = state.defaultMode.ApplyMode()
	if err != nil {
		return "", err
	}
	return line, err
}

func LogServerMain(args []string, ctx context.Context) error {
	fset := flag.NewFlagSet("logserver", flag.ContinueOnError)
	remotes := fset.String("remotes", "", "remote hosts")
	err := fset.Parse(args)
	if err != nil {
		return err
	}

	var remoteList []string
	if *remotes != "" {
		remoteList = strings.Split(*remotes, ",")
	}
	for _, remote := range remoteList {
		cmd := exec.Command(
			"ssh", "-N",
			"-o", "ExitOnForwardFailure=yes",
			"-R", "5678:localhost:5678", remote)
		err := cmd.Start()
		if err != nil {
			return err
		}
		defer cmd.Process.Kill()
	}

	listener, err := net.Listen("tcp", "localhost:5678")
	if err != nil {
		return err
	}
	defer listener.Close()

	conns := make(chan net.Conn)
	go Accept(listener, conns)

	lines := make(chan string)
	go PrintLines(lines)
	for {
		select {
		case conn := <-conns:
			defer conn.Close()
			go ReadLines(conn, lines)
		case <-ctx.Done():
			log.Println("Cancelling")
			return nil
		}
	}
}

func Accept(listener net.Listener, dst chan net.Conn) error {
	for {
		conn, err := listener.Accept()
		if err != nil {
			return err
		}
		dst <- conn
	}
}

func PrintLines(lines chan string) {
	for line := range lines {
		fmt.Println(line)
	}
}

func ReadLines(conn net.Conn, lines chan string) error {
	reader := bufio.NewReader(conn)
	for {
		line, _, err := reader.ReadLine()
		if err != nil {
			log.Println(err)
			return err
		}
		fmt.Println(string(line))
	}
}

func ReadJsons(conn net.Conn, dst chan Message) error {
	reader := json.NewDecoder(conn)
	for {
		var msg Message
		err := reader.Decode(&msg)
		if err != nil {
			log.Println(err)
			return err
		}
		dst <- msg
	}
}

func RandomName() string {
	words := []string{
		"ocean", "mountain", "cat", "computer", "river",
		"painting", "novel", "piano", "lawyer", "bacteria",
		"galaxy", "equation", "volcano", "poetry",
		"theater", "neuron", "atom", "eclipse", "typhoon",
		"quasar", "jazz", "chess", "robot", "cipher",
		"sonnet", "monsoon", "kaleidoscope", "harmony", "jungle",
	}
	var result []string
	for i := 0; i < 3; i++ {
		result = append(result, words[rand.Intn(len(words))])
	}
	return strings.Join(result, "-")
}

type Message = struct {
	Type    string
	Payload string
}

type State = struct {
	runner      *interp.Runner
	lnr         *liner.State
	defaultMode liner.ModeApplier
	linerMode   liner.ModeApplier
}
