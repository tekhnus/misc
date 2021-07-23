package euler

func Reverse(s string) string {
	length := len(s)
	result := make([]byte, length)
	for index, byte := range []byte(s) {
		result[length - 1 - index] = byte
	}
	return string(result)
}
