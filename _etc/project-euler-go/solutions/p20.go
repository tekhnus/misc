package solutions

import "github.com/tekhnus/project-euler-go"

func P20() int {
	return int(euler.Sum(euler.Digits(euler.Factorial(100))))
}
