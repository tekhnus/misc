package euler

func WriteFarey(n int64, ch chan Rat64) {
	var a, b int64 = 0, 1
	var c, d int64 = 1, n
	for c <= n {
		k := (n + b) / d
		a, b, c, d = c, d, k*c-a, k*d-b
		ch <- Rat64{a, b}
	}
	close(ch)
}
