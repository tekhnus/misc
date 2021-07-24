package euler

func LinearSlice(grid [][]int64, i int, j int, di int, dj int) []int64 {
	result := []int64{}
	for i >= 0 && i < len(grid) && j >= 0 && j < len(grid[i]) {
		result = append(result, grid[i][j])
		i += di
		j += dj
	}
	return result
}

func Product(seq []int64) int64 {
	var result int64 = 1
	for _, x := range seq {
		result *= x
	}
	return result
}
