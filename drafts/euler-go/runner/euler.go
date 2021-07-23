package main

import "fmt"
import "os"
import "github.com/tekhnus/euler-go/solutions"

var solutionById = map[string]func()int {
	"P1": solutions.P1,
	"P2": solutions.P2,
	"P3": solutions.P3,
}

func main() {
	problemId := os.Args[1]
	solution := solutionById[problemId]
	fmt.Println(solution())
}
