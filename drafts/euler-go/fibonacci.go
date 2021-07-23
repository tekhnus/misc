package euler

func WriteFibonacci(c chan int) {
	a := 0
	b := 1
	for {
		c <- a
		a, b = b, a+b
	}
}
