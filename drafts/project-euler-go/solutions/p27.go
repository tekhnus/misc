package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

func P27() int64 {
	primec := make(chan int)
	go euler.WritePrimes(primec)

	isprime := make(map[int64]bool)
	for p := range primec {
		if p > 4000000 {
			break
		}
		isprime[int64(p)] = true
	}

	pairs := make(chan euler.V2I64, 4000000)
	go euler.WritePairs(pairs, -999, 1000, -1000, 1001)

	lengths := make(chan euler.V3I64)
	var wg sync.WaitGroup
	euler.RunMany(2, &wg, func() {
		var best euler.V3I64
		for p := range pairs {
			n := primeCombo(p, isprime)
			if n > best.I {
				best = euler.V3I64{n, p.I, p.J}
			}
		}
		if best.I > 0 {
			lengths <- best
		}
	})
	go euler.Closer(lengths, &wg)

	best := euler.ReadMaxV3I64(lengths)
	return best.J * best.K
}

func primeCombo(coef euler.V2I64, isprime map[int64]bool) int64 {
	var n int64
	for isprime[n*n+coef.I*n+coef.J] {
		n++
	}
	return n
}
