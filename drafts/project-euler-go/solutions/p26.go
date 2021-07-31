package solutions

import "github.com/tekhnus/project-euler-go"

func P26() int64 {
	var longest int64 = -1
	var best int64
	for d := int64(2); d < 1000; d++ {
		_, n := decompose(10, d)
		if n > longest {
			longest = n
			best = d
		}
	}
	return best
}

func decompose(base int64, d int64) (int64, int64) {
	/*
	   Given base > 1 and d > 0,
	   return the smallest pair (x, y)
	   with x >= 0 and y > 0
	   such that base^x * (base^y - 1) divides d.
	*/
	if base <= 1 || d <= 0 {
		panic("Wrong call")
	}
	f := euler.PrimeFactors(base)
	di, dc := euler.PartiallyFactor(f, d) // so d == Val(f, di) * dc
	x := euler.MaxSliceI64(di)            // so base^x divides Val(f, di)
	y := euler.DiscLogUnit(base, dc)      // so base^y = 1 modulo dc, thus (base^y - 1) divides dc
	return x, y
}
