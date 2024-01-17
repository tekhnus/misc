package solutions

import _ "embed"
import "strings"
import "log"
import "github.com/tekhnus/project-euler-go"

//go:embed p11.txt
var p11txt string

var size = 20
var directions = []euler.Vec2D{
	{1, 0}, {0, 1}, {1, 1}, {1, -1},
}

func P11() int {
	stream := strings.NewReader(p11txt)
	grid := make([][]int64, size)

	for i := range grid {
		grid[i] = make([]int64, size)
		err := euler.FscanlnDecimals64(stream, grid[i])
		if err != nil {
			log.Fatal(err)
		}
	}

	segments := make(chan []euler.Vec2D)
	go func() {
		euler.GridEach(grid, func(p euler.Vec2D) {
			for _, d := range directions {
				segments <- euler.GridSegment(p, d, 4)
			}
		})
		close(segments)
	}()

	products := make(chan int)
	go func() {
		for seg := range segments {
			val, err := euler.GridGet(grid, seg)
			if err {
				continue
			}
			products <- int(euler.Product(val))
		}
		close(products)
	}()

	return euler.ReadMax(products)
}
