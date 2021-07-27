package solutions

import _ "embed"
import "strings"
import "github.com/tekhnus/project-euler-go"

//go:embed p18.txt
var p18txt string

func P18() int64 {
	input := readInput()
	best := make([][]int64, 15)
	for i := 0; i < 15; i++ {
		best[i] = make([]int64, i+1)
		for j := 0; j <= i; j++ {
			best[i][j] = calc(best, i, j, input)
		}
	}
	return MaxI64(best[len(best)-1])
}

func readInput() [][]int64 {
	d := make([][]int64, 15)
	stream := strings.NewReader(p18txt)
	for i := 0; i < 15; i++ {
		d[i] = make([]int64, i+1)
		euler.FscanlnDecimals64(stream, d[i])
	}
	return d
}

func calc(best [][]int64, i int, j int, input [][]int64) int64 {
	if i == 0 {
		return input[0][0]
	}
	var m int64 = -1000000
	if j > 0 {
		x := best[i-1][j-1]
		if x > m {
			m = x
		}
	}
	if j < len(best[i-1]) {
		x := best[i-1][j]
		if x > m {
			m = x
		}
	}
	return m + input[i][j]
}

func MaxI64(s []int64) int64 {
	m := s[0]
	for _, v := range s {
		if v > m {
			m = v
		}
	}
	return m
}
