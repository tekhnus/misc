package solutions

import "github.com/tekhnus/project-euler-go"

func P7() int {
	ch := make(chan int)
	go euler.WritePrimes(ch)
	for i := 1; i <= 10000; i++ {
		<-ch
	}
	return <-ch
}
