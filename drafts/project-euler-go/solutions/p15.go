package solutions

const n = 20

func P15() int64 {
	var ans [n + 1][n + 1]int64
	for i := n; i >= 0; i-- {
		for j := n; j >= 0; j-- {
			ans[i][j] = compute(&ans, i, j)
		}
	}
	return ans[0][0]
}

func compute(ans *[n + 1][n + 1]int64, i int, j int) int64 {
	if i == n || j == n {
		return 1
	}
	down := ans[i+1][j]
	right := ans[i][j+1]
	if down == 0 || right == 0 {
		panic("wrong computation order")
	}
	return down + right
}
