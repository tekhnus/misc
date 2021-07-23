package main

import "fmt"
import "github.com/tekhnus/euler-go/share"

func main() {
	fib := make(chan int)
	go share.WriteFibonacci(fib)
	sum := 0
	for x := range fib {
		if x > 4000000 {
			break
		}
		if x % 2 == 0 {
			sum += x
		}
	}
	fmt.Println(sum)
}
