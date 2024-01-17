package solutions

import _ "embed"
import "strings"
import "github.com/tekhnus/project-euler-go"

//go:embed p8.txt
var input string

const length = 13

func P8() int {
	input = strings.ReplaceAll(input, "\n", "")

	subseqs := make(chan string)
	products := make(chan int)

	go func() {
		for s := range subseqs {
			prod := 1
			for _, char := range s {
				prod *= int(char - '0')
			}
			products <- prod
		}
		close(products)
	}()

	go func() {
		lastStart := len(input) - length
		for start := 0; start <= lastStart; start++ {
			subseqs <- input[start : start+length]
		}
		close(subseqs)
	}()

	return euler.ReadMax(products)
}
