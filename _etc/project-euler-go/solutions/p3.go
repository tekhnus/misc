package solutions

import "github.com/tekhnus/project-euler-go"

func P3() int {
	n := 600851475143
	fac := make(chan int)
	go euler.WritePrimeFactors(n, fac)
	maxFac := euler.ReadMax(fac)
	return maxFac
}
