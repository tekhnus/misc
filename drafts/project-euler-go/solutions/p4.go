package solutions

import "strconv"
import "github.com/tekhnus/project-euler-go"

func P4() int {
	ch := make(chan int)
	go func() {
		for a := 100; a <= 999; a++ {
			for b := 100; b <= 999; b++ {
				product := a * b
				s := strconv.Itoa(product)
				if s == euler.Reverse(s) {
					ch <- product
				}
			}
		}
		close(ch)
	}()
	return euler.ReadMax(ch)
}
