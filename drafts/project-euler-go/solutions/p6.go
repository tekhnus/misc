package solutions

func P6() int {
	result := 0
	for a := 1; a <= 100; a++ {
		for b := 1; b <= 100; b++ {
			if a == b {
				continue
			}
			result += a * b
		}
	}
	return result
}
