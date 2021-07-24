package solutions

import _ "embed"
import "strings"

import "log"
import "github.com/tekhnus/project-euler-go"

//import "fmt"
//go:embed p11.txt
var p11txt string

func P11() int {
	stream := strings.NewReader(p11txt)
	grid := make([][]int64, 20)

	for i := range grid {
		grid[i] = make([]int64, 20)
		err := euler.FscanlnDecimals64(stream, grid[i])
		if err != nil {
			log.Fatal(err)
		}
	}

	ch := make(chan []int64)

	go func() {
		for i := 0; i < 20; i++ {
			for j := 0; j < 20; j++ {
				ch <- euler.LinearSlice(grid, i, j, 0, 1)
				ch <- euler.LinearSlice(grid, i, j, 1, 0)
				ch <- euler.LinearSlice(grid, i, j, 1, 1)
				ch <- euler.LinearSlice(grid, i, j, 1, -1)
			}
		}
		close(ch)
	}()

	prods := make(chan int)

	go func() {
		for seg := range ch {
			if len(seg) >= 4 {
				//fmt.Println(euler.Product(seg[:4]))
				prods <- int(euler.Product(seg[:4]))
			}
		}
		close(prods)
	}()

	return euler.ReadMax(prods)
}
