package euler

import "math/big"

func Digits(x big.Int) []int {
	s := x.String()
	d := make([]int, len(s))
	for i, c := range s {
		d[i] = int(c - '0')
	}
	return d
}

func Factorial(x int64) big.Int {
	var fac big.Int
	fac.SetInt64(1)
	for n := int64(1); n <= x; n++ {
		var nn big.Int
		nn.SetInt64(n)
		fac.Mul(&fac, &nn)
	}
	return fac
}
