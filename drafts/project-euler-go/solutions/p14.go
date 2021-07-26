package solutions

import "sync"
import "github.com/tekhnus/project-euler-go"

type pair euler.V2I64

func (a pair) CompareTo(b euler.Comparable) int {
	return euler.CompareI64(euler.V2I64(a).J, euler.V2I64(b.(pair)).J)
}

const nworkers = 3

func P14() int64 {
	numbers := make(chan int64)
	go euler.WriteIntegers(numbers, 1, 1000000)
	lengths := make(chan euler.Comparable)
	var wg sync.WaitGroup
	wg.Add(nworkers)
	for i := 0; i < nworkers; i++ {
		go WriteCollatzLengths(lengths, numbers, &wg)
	}
	go euler.Closer(lengths, &wg)
	max := euler.ReadMaxGeneric(lengths)
	return max.(pair).I
}

func WriteCollatzLengths(outp chan euler.Comparable, inp chan int64, wg *sync.WaitGroup) {
	for i := range inp {
		outp <- pair{i, CollatzLength(i)}
	}
	wg.Done()
}

func CollatzLength(n int64) int64 {
	var len int64 = 0
	for ; n != 1; n = CollatzNext(n) {
		len++
	}
	return len
}

func CollatzNext(n int64) int64 {
	if n % 2 == 0 {
		return n / 2
	}
	return n * 3 + 1
}
