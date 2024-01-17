package solutions

import "github.com/tekhnus/project-euler-go"
import "math/big"

func P25() int {
	c := make(chan big.Int)
	go euler.WriteFibonacciBig(c)
	i := 0
	for x := range c {
		if len(x.String()) >= 1000 {
			return i
		}
		i++
	}
	return 0
}
