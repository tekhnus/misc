package euler

func ReadMax(c chan int) int {
	max := <- c
	for v := range(c) {
		if v > max {
			max = v
		}
	}
	return max
}
