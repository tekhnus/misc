package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

type p27res struct{ I, J, N int64 }

func (x p27res) CompareTo(y euler.Comparable) int {
	return euler.CompareI64(x.N, y.(p27res).N)
}

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

	pairs := make(chan euler.V2I64)
	go euler.WritePairs(pairs, -999, 1000, -1000, 1001)

	lengths := make(chan euler.Comparable, 4000000)
	var wg sync.WaitGroup
	euler.RunMany(3, &wg, func() {
		for p := range pairs {
			n := primeCombo(p, isprime)
			lengths <- p27res{p.I, p.J, n}
		}
	})
	go euler.Closer(lengths, &wg)

	best := euler.ReadMaxGeneric(lengths).(p27res)
	return best.I * best.J
}

func primeCombo(coef euler.V2I64, isprime map[int64]bool) int64 {
	var n int64
	for isprime[n*n+coef.I*n+coef.J] {
		n++
	}
	return n
}
