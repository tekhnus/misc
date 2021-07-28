package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

func P23() int {
	nums := make(chan int64)
	go euler.WriteIntegers(nums, 1, 30000)

	abu := make(chan int64)
	var wg sync.WaitGroup
	euler.RunMany(3, &wg, func() {
		for x := range nums {
			if isAbundant(x) {
				abu <- x
			}
		}
	})
	go euler.Closer(abu, &wg)

	ab := euler.ReadSliceI64(abu)

	abu = make(chan int64)
	go euler.WriteSliceI64(abu, ab)

	var repr [30000]bool
	euler.RunMany(4, &wg, func() {
		for n := range abu {
			for _, m := range ab {
				s := m + n
				if s < 30000 {
					repr[s] = true
				}
			}
		}
	})

	wg.Wait()
	res := 0
	for i := 1; i < 30000; i++ {
		if !repr[i] {
			res += i
		}
	}
	return res
}

func isAbundant(n int64) bool {
	divs := make(chan int)
	go euler.WriteFactors(int(n), divs)
	sum := 1 + euler.ReadSum(divs)
	return sum > int(n)
}
