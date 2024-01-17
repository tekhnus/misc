package solutions

import _ "embed"
import "strings"
import "fmt"
import "log"
import "sort"

//go:embed p22.txt
var p22txt string

func P22() int {
	names := parseInp()
	sort.Strings(names)
	s := 0
	for i := range names {
		s += score(names, i)
	}
	return s
}

func parseInp() []string {
	var names []string
	stream := strings.NewReader(p22txt)
	for {
		var name string
		_, err := fmt.Fscanf(stream, "%q", &name)
		if err != nil {
			log.Fatal(err)
		}
		names = append(names, name)
		_, err = fmt.Fscanf(stream, ",")
		if err != nil {
			break
		}
	}
	_, err := fmt.Fscanf(stream, "\n")
	if err != nil {
		log.Fatal(err)
	}
	return names
}

func score(names []string, i int) int {
	return (i + 1) * namescore(names[i])
}

func namescore(name string) int {
	score := 0
	for _, c := range name {
		score += 1 + int(c-'A')
	}
	return score
}
