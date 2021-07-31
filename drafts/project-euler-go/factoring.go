package euler

import "math"
import "math/big"

func WriteFactors(n int, c chan int) {
	var upperBound int = int(math.Sqrt(float64(n)))
	for m := 2; m < upperBound; m++ {
		if n%m == 0 {
			c <- m
			quotient := n / m
			c <- quotient
		}
	}
	if n%upperBound == 0 && upperBound > 1 {
		c <- upperBound
		quotient := n / upperBound
		if quotient != upperBound {
			c <- quotient
		}
	}
	close(c)
}

func IsPrime(n int) bool {
	if n == 1 {
		return true
	}
	ch := make(chan int)
	go WriteFactors(n, ch)
	count := 0
	for _ = range ch {
		count++
	}
	return count == 0
}

func WritePrimeFactors(n int, c chan int) {
	ch := make(chan int)
	go WriteFactors(n, ch)
	for x := range ch {
		if IsPrime(x) {
			c <- x
		}
	}
	close(c)
}

func WritePrimes(c chan int) {
	primes := []int{}
	for p := 2; ; p++ {
		isPrime := true
		var upperBound int = int(math.Sqrt(float64(p)))
		for _, r := range primes {
			if r > upperBound {
				break
			}
			if p%r == 0 {
				isPrime = false
				break
			}
		}
		if isPrime {
			c <- p
			primes = append(primes, p)
		}
	}
}

func LCM(z *big.Int, a *big.Int, b *big.Int) {
	// An temporary variable is allocated,
	// because `z` can coincide with `a` or `b`.
	var tmp big.Int
	tmp.GCD(nil, nil, a, b)
	tmp.Div(a, &tmp)
	tmp.Mul(&tmp, b)
	z.Set(&tmp)
}

func PrimeFactors(n int64) []int64 {
	c := make(chan int)
	go WritePrimeFactors(int(n), c)
	var res []int64
	for f := range c {
		res = append(res, int64(f))
	}
	return res
}

func PartiallyFactor(fs []int64, n int64) ([]int64, int64) {
	ind := make([]int64, len(fs))
	for i, f := range fs {
		for n%f == 0 {
			ind[i]++
			n /= f

		}
	}
	return ind, n
}
