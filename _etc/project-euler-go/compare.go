package euler

type Comparable interface {
	CompareTo(b Comparable) int
}

func CompareI64(a, b int64) int {
	if a > b {
		return +1
	}
	if a < b {
		return -1
	}
	return 0
}

func MaxSliceI64(s []int64) int64 {
	m := s[0]
	for _, v := range s {
		if v > m {
			m = v
		}
	}
	return m
}

func SetMaxI64(x *int64, y int64) {
	if y > *x {
		*x = y
	}
}

func SetMaxV3I64(acc *V3I64, val V3I64) {
	if val.I > acc.I {
		*acc = val
	}
}
