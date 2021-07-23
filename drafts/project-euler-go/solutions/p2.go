package solutions

import "github.com/tekhnus/project-euler-go"

func P2() int {
	fib := make(chan int)
	go euler.WriteFibonacci(fib)
	sum := 0
	for x := range fib {
		if x > 4000000 {
			break
		}
		if x%2 == 0 {
			sum += x
		}
	}
	return sum
}
