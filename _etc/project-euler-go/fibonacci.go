package euler

import "math/big"

func WriteFibonacci(c chan int) {
	a := 0
	b := 1
	for {
		c <- a
		a, b = b, a+b
	}
}

func WriteFibonacciBig(c chan big.Int) {
	var a, b big.Int
	b.SetInt64(1)
	for {
		var x big.Int
		x.Set(&a)
		c <- x
		a.Add(&a, &b)
		a, b = b, a
	}
}
