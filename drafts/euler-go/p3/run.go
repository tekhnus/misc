package main

import "fmt"
import "github.com/tekhnus/euler-go/share"

func main() {
	n := 600851475143
	fac := make(chan int)
	go share.WritePrimeFactors(n, fac)
	maxFac := share.ReadMax(fac)
	fmt.Println(maxFac)
}

