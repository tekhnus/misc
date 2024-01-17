package solutions

import "github.com/tekhnus/project-euler-go"
import "strings"
import "strconv"

func P24() string {
	d := nats(10)

	var ans string
	i := 0
	euler.Permute(d, func() int {
		i++
		if i == 1000000 {
			ans = strings.Join(d, "")
			return euler.Break
		}
		return euler.Continue
	})

	return ans
}

func nats(m int) []string {
	d := make([]string, 10)
	for n := 0; n < m; n++ {
		d[n] = strconv.Itoa(n)
	}
	return d
}
