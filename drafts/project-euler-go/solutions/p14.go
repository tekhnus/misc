package solutions

import "github.com/tekhnus/project-euler-go"

func P14() int64 {
	numbers := make(chan int64)
	go WriteIntegers(numbers, 1, 1000000)
	lengths := make(chan euler.V2I64)
	go WriteCollatzLengths(lengths, numbers)
	max := ReadMaxJ(lengths)
	return max.I
}

func WriteIntegers(c chan int64, from int64, to int64) {
	for i := from; i < to; i++ {
		c<-i
	}
	close(c)
}

func WriteCollatzLengths(outp chan euler.V2I64, inp chan int64) {
	for i := range inp {
		outp <- euler.V2I64{i, CollatzLength(i)}
	}
	close(outp)
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

