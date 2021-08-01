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
	if n <= 0 {
		return false
	}
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

func WritePrimesUpTo(c chan int, bound int) {
	primes := []int{}
	for p := 2; p < bound; p++ {
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
	close(c)
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

func PrimeDecomposition(n int64) ([]int64, []int64) {
	/*
	   Return the list of all prime factors of n
	   and the list of the respective multiplicities (i.e. p-adic orders)
	*/
	c := make(chan int)
	go WritePrimeFactors(int(n), c)
	var res []int64
	for f := range c {
		res = append(res, int64(f))
	}
	ix, rest := PartiallyFactor(res, n)
	if rest != 1 {
		panic("implementation failure")
	}
	return res, ix
}

func PartiallyFactor(fs []int64, n int64) ([]int64, int64) {
	/*
	   Given a list of distinct prime numbers
	   and a number n, return the list of the respective
	   multiplicities of prime factors in n,
	   and the maximal divisor of n which is coprime to all listed primes.
	*/
	ind := make([]int64, len(fs))
	for i, f := range fs {
		for n%f == 0 {
			ind[i]++
			n /= f

		}
	}
	return ind, n
}

func Log(x []int64, b []int64) int64 {
	/*
	   Assuming that x and b encode numbers
	   by listing their p-adic orders wrt
	   the common list of primes,
	   return the ceil of the logarithm of x with base b.
	*/
	var res int64 = 1
	for i, xi := range x {
		bi := b[i]
		r := xi / bi
		if r > res {
			res = r
		}
	}
	return res
}
