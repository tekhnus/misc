package solutions

import "sync"
import "reflect"
import "github.com/tekhnus/project-euler-go"

const nworkers = 3

func P14() int64 {
	numbers := make(chan int64)
	go WriteIntegers(numbers, 1, 1000000)
	lengths := make(chan euler.V2I64)
	var wg sync.WaitGroup
	wg.Add(nworkers)
	for i := 0; i < nworkers; i++ {
		go WriteCollatzLengths(lengths, numbers, &wg)
	}
	go Closer(lengths, &wg)
	max := ReadMaxJ(lengths)
	return max.I
}

func Closer(c interface{}, wg *sync.WaitGroup) {
	wg.Wait()
	reflect.ValueOf(c).Close()
}

func WriteIntegers(c chan int64, from int64, to int64) {
	for i := from; i < to; i++ {
		c<-i
	}
	close(c)
}

func WriteCollatzLengths(outp chan euler.V2I64, inp chan int64, wg *sync.WaitGroup) {
	for i := range inp {
		outp <- euler.V2I64{i, CollatzLength(i)}
	}
	wg.Done()
}

func ReadMaxJ(inp chan euler.V2I64) euler.V2I64 {
	result := <- inp
	for p := range inp {
		if p.J > result.J {
			result = p
		}
	}
	return result
}

func CollatzLength(n int64) int64 {
	var len int64 = 0
	for n != 1 {
		n = CollatzNext(n)
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

