package solutions

func P17() int64 {
	ch := make(chan []string)
	go func() {
		for n := int64(1); n <= 1000; n++ {
			ch <- Spell(n)
		}
		close(ch)
	}()
	var symbols int64
	for number := range ch {
		for _, word := range number {
			symbols += int64(len(word))
		}
	}
	return symbols
}

func Spell(n int64) []string {
	if n <= 9 {
		return [][]string{
			{"one"},
			{"two"},
			{"three"},
			{"four"},
			{"five"},
			{"six"},
			{"seven"},
			{"eight"},
			{"nine"}}[n-1]
	}
	if n <= 19 {
		return [][]string{
			{"ten"},
			{"eleven"},
			{"twelve"},
			{"thirteen"},
			{"fourteen"},
			{"fifteen"},
			{"sixteen"},
			{"seventeen"},
			{"eighteen"},
			{"nineteen"}}[n-10]
	}
	if n <= 99 && n%10 == 0 {
		return [][]string{
			{"twenty"},
			{"thirty"},
			{"forty"},
			{"fifty"},
			{"sixty"},
			{"seventy"},
			{"eighty"},
			{"ninety"}}[(n-20)/10]
	}
	if n <= 99 {
		result := Spell(n - n%10)
		if n%10 != 0 {
			result = append(result, Spell(n%10)...)
		}
		return result
	}
	if n <= 999 {
		result := append(Spell(n/100), "hundred")
		if n%100 != 0 {
			result = append(result, "and")
			result = append(result, Spell(n%100)...)
		}
		return result
	}
	if n == 1000 {
		return []string{"one", "thousand"}
	}
	panic("unsupported")
}
