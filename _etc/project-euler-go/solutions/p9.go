package solutions

import "github.com/tekhnus/project-euler-go"

func P9() int {
	ch := make(chan euler.Rat64)
	go euler.WriteFarey(32, ch)
	for p := range ch {
		if p.N >= p.D {
			break
		}
		if p.N%2 == 1 && p.D%2 == 1 {
			continue
		}
		a := 2 * p.D * p.N
		b := p.D*p.D - p.N*p.N
		c := p.D*p.D + p.N*p.N
		s := a + b + c
		if 1000%s == 0 {
			k := 1000 / s
			return int((k * a) * (k * b) * (k * c))
		}
	}
	panic("No such triple")
}
