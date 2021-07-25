package solutions

import "github.com/tekhnus/project-euler-go"

func P12() int {
	for n := int64(2); ; n++ {
		div := make(chan int)
		t := euler.Triangular(n)
		go euler.WriteFactors(int(t), div)
		cnt := 2 + euler.ReadCount(div)
		if cnt > 500 {
			return int(t)
		}
	}
}
