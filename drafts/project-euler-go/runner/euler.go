package main

import "fmt"
import "os"
import "reflect"
import "github.com/tekhnus/project-euler-go/solutions"

func main() {
	problemId := os.Args[1]
	solution := solutions.SolutionIndex[problemId]
	fmt.Println(reflect.ValueOf(solution).Call([]reflect.Value{})[0])
}
