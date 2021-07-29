package solutions

import "fmt"

func P26() int {
	longest := -1
	best := 0
	for d := 2; d < 1000; d++ {
		_, cycle := ToDecimal(1, d)
		n := len(cycle)
		//fmt.Println(d, n)
		if n > longest {
			longest = n
			best = d
		}
	}
	return best
	/*
		10^t * 1 / d = _ / (10^k - 1)
		10^t * (10^k - 1) = _ * d
		d' := d wo 2s and 5s
		10^k - 1 = 0 (mod d')
		10^k = 1 (mod d')
	*/
}

func ToDecimal(n int, d int) (string, string) {
	if n < 0 || d <= 0 || n >= d {
		panic("Wrong arguments for ToDecimal")
	}
	a, b, k, t := ToABKT(n, d)
	init := fmt.Sprintf("%0*d", t, a)
	cycle := fmt.Sprintf("%0*d", k, b)
	return init, cycle
}

func ToABKT(n int, d int) (int, int, int, int) {
	/*
	   n / d = a / 10^t + b / (10^t * (10^k - 1))
	   n / d = (a * (10^k - 1) + b) / (10^t * (10^k - 1))
	*/
	dd := d
	for dd%2 == 0 {
		dd /= 2
	}
	for dd%5 == 0 {
		dd /= 5
	}
	k := 1
	tenk := 10
	if dd != 1 {
		for tenk%dd != 1 {
			k++
			tenk *= 10
			tenk = tenk % dd
		}
	}
	return 0, 0, k, 0
}
