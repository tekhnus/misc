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
