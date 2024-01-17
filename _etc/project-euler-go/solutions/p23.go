package solutions

import "github.com/tekhnus/project-euler-go"
import "sync"

const nn = 30000

func P23() int {
	nums := make(chan int64)
	go euler.WriteIntegers(nums, 1, nn)

	abu := make(chan int64)
	go writeAbundant(abu, nums)

	ab := euler.ReadSliceI64(abu)
	repr := computeRepr(ab)

	res := 0
	for i := 1; i < nn; i++ {
		if !repr[i] {
			res += i
		}
	}
	return res
}

func writeAbundant(abu chan int64, nums chan int64) {
	var wg sync.WaitGroup
	euler.RunMany(3, &wg, func() {
		for x := range nums {
			if isAbundant(x) {
				abu <- x
			}
		}
	})
	go euler.Closer(abu, &wg)
}

func computeRepr(ab []int64) []bool {
	abu := make(chan int64)
	go euler.WriteSliceI64(abu, ab)

	var repr [nn]bool
	var wg sync.WaitGroup
	euler.RunMany(4, &wg, func() {
		for k := range abu {
			for _, m := range ab {
				s := m + k
				if s < nn {
					repr[s] = true
				}
			}
		}
	})
	wg.Wait()

	return repr[:]
}

func isAbundant(nn int64) bool {
	divs := make(chan int)
	go euler.WriteFactors(int(nn), divs)
	sum := 1 + euler.ReadSum(divs)
	return sum > int(nn)
}
