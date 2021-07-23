package main

import "fmt"
import "os"
import "github.com/tekhnus/project-euler-go/solutions"

func main() {
	problemId := os.Args[1]
	solution := solutions.SolutionIndex[problemId]
	fmt.Println(solution())
}
