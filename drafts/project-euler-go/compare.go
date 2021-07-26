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
