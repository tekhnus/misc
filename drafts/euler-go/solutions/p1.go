package solutions

func P1() int {
	sum := 0
	for x := 0; x < 1000; x++ {
		if x % 3 == 0 || x % 5 == 0 {
			sum += x
		}
	}
	return sum
}
