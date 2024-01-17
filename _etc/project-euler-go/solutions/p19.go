package solutions

import "time"

func P19() int {
	ans := 0
	d := time.Date(1901, time.January, 1, 0, 0, 0, 0, time.UTC)
	for ; d.Year() <= 2000; d = d.AddDate(0, 0, 1) {
		if d.Day() == 1 && d.Weekday() == time.Sunday {
			ans++
		}
	}
	return ans
}
