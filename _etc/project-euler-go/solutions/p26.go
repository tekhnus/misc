package solutions

import "github.com/tekhnus/project-euler-go"

func P26() int64 {
	var longest int64 = -1
	var best int64
	for d := int64(2); d < 1000; d++ {
		_, n := prefixAndCycleLengths(10, d)
		if n > longest {
			longest = n
			best = d
		}
	}
	return best
}

func prefixAndCycleLengths(base int64, d int64) (int64, int64) {
	/*
	   Given base > 1 and d > 0,
	   return the prefix and the cycle length
	   of the representation of 1 / d in the given base.

	   To do this, we compute the smallest pair (x, y)
	   with x >= 0 and y > 0
	   such that base^x * (base^y - 1) divides d.
	*/
	if base <= 1 || d <= 0 {
		panic("Wrong call")
	}
	f, basei := euler.PrimeDecomposition(base)
	di, dc := euler.PartiallyFactor(f, d)
	x := euler.Log(di, basei)        // so base^x divides di
	y := euler.DiscLogUnit(base, dc) // so base^y = 1 modulo dc, thus (base^y - 1) divides dc
	return x, y
}
