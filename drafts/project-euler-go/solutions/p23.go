package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

func P23() int {
	nums := make(chan int64)
	go euler.WriteIntegers(nums, 1, 30000)

	abu := make(chan int64)
	nworkers := 3
	var wg sync.WaitGroup
	wg.Add(nworkers)
	for i := 0; i < nworkers; i++ {
		go func() {
			for x := range nums {
				if isAbundant(x) {
					abu <- x
				}
			}
			wg.Done()
		}()
	}
	go euler.Closer(abu, &wg)

	var ab []int64
	for x := range abu {
		ab = append(ab, x)
	}

	var repr [30000]bool
	for _, n := range ab {
		for _, m := range ab {
			s := m + n
			if s < 30000 {
				repr[s] = true
			}
		}
	}

	res := 0
	for i, r := range repr {
		if !r {
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
