package solutions

import "github.com/tekhnus/project-euler-go"

func P10() int {
	ch := make(chan int)
	go euler.WritePrimes(ch)
	s := 0
	for p := range ch {
		if p >= 2000000 {
			break
		}
		s += p
	}
	return s
}
