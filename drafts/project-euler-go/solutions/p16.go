package solutions

import "math/big"

func P16() int64 {
	var two big.Int
	two.SetInt64(2)
	var thousand big.Int
	thousand.SetInt64(1000)
	var x big.Int
	x.Exp(&two, &thousand, nil)
	return sumDigits(x)
}

func sumDigits(x big.Int) int64 {
	s := x.String()
	var sum int64
	for _, c := range s {
		sum += int64(c - '0')
	}
	return sum
}
