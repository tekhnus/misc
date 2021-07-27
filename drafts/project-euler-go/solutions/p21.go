package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

const numworkers = 4

func P21() int {
	numbers := make(chan int64)
	go euler.WriteIntegers(numbers, 2, 10000)
	var divsum [10000]int
	var wg sync.WaitGroup
	wg.Add(numworkers)
	for i := 0; i < numworkers; i++ {
		go func() {
			for n := range numbers {
				divs := make(chan int)
				go euler.WriteFactors(int(n), divs)
				divsum[n] = 1 + euler.ReadSum(divs)
			}
			wg.Done()
		}()
	}
	wg.Wait()

	s := 0
	for i := 2; i < 10000; i++ {
		j := divsum[i]
		if j < 10000 && j > 1 && divsum[j] == i && i != j {
			s += i
		}
	}
	return s
}
