package euler

func Reverse(s string) string {
	length := len(s)
	result := make([]byte, length)
	for index, byte := range []byte(s) {
		result[length-1-index] = byte
	}
	return string(result)
}

func IsPalindromic(s string) bool {
	left := 0
	right := len(s) - 1
	for left < right {
		if s[left] != s[right] {
			return false
		}
		left++
		right--
	}
	return true
}
