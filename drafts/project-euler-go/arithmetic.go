package euler

func DiscLogUnit(b int64, m int64) int64 {
	/*
	   Given a pair of coprime positive numbers b and m,
	   return the discrete logarithm of 1 with base b modulo m,
	   that is, the least positive n
	   such that b^n == 1 modulo m.
	*/
	if b <= 0 || m <= 0 {
		panic("Wrong call")
	}
	if m == 1 {
		return 1
	}
	var n int64 = 1
	var bn int64 = b
	for bn != 1 {
		n++
		bn = (bn * b) % m
	}
	return n
}
