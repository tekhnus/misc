package euler

func ReadMax(c chan int) int {
	max := <-c
	for v := range c {
		if v > max {
			max = v
		}
	}
	return max
}

func ReadCount(c chan int) int {
	count := 0
	for _ = range c {
		count++
	}
	return count
}
