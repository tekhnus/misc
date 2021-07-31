package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

func P27() int64 {
	pairs := make(chan euler.V2I64)
	go euler.WritePairs(pairs, -999, 1000, -1000, 1001)

	lengths := make(chan struct{ I, J, N int64 })
	var wg sync.WaitGroup
	euler.RunMany(2, &wg, func() {
		for p := range pairs {
			lengths <- struct{ I, J, N int64 }{p.I, p.J, primeCombo(p)}
		}
	})
	go euler.Closer(lengths, &wg)
	var bestcombo int64
	var bestproduct int64
	for x := range lengths {
		if x.N > bestcombo {
			bestcombo = x.N
			bestproduct = x.I * x.J
		}
	}
	return bestproduct
}

func primeCombo(coef euler.V2I64) int64 {
	var n int64
	for euler.IsPrime(int(n*n + coef.I*n + coef.J)) {
		n++
	}
	return n
}
