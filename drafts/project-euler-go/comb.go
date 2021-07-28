package euler

const (
	Continue = iota
	Break
)

func Permute(seq []string, f func() int) int {
	if len(seq) == 0 {
		return f()
	}
	var i int
	brk := Continue
	for i = range seq {
		seq[0], seq[i] = seq[i], seq[0]
		brk := Permute(seq[1:], f)
		if brk == Break {
			break
		}
	}
	for j := 0; j < i; j++ {
		seq[j], seq[j+1] = seq[j+1], seq[j]
	}
	return brk
}
