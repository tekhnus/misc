package euler

func Product(seq []int64) int64 {
	var result int64 = 1
	for _, x := range seq {
		result *= x
	}
	return result
}

func Sum(seq []int) int64 {
	var result int64 = 0
	for _, x := range seq {
		result += int64(x)
	}
	return result
}
