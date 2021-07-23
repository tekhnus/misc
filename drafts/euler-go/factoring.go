package euler

import "math"

func WriteFactors(n int, c chan int) {
	var upperBound int = int(math.Sqrt(float64(n)))
	for m := 2; m < upperBound; m++ {
		if n%m == 0 {
			c <- m
			quotient := n / m
			c <- quotient
		}
	}
	if n%upperBound == 0 {
		c <- upperBound
	}
	close(c)
}

func IsPrime(n int) bool {
	if n == 1 {
		return true
	}
	ch := make(chan int)
	go WriteFactors(n, ch)
	count := 0
	for _ = range ch {
		count++
	}
	return count == 0
}

func WritePrimeFactors(n int, c chan int) {
	ch := make(chan int)
	go WriteFactors(n, ch)
	for x := range ch {
		if IsPrime(x) {
			c <- x
		}
	}
	close(c)
}
