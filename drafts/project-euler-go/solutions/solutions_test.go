package solutions

import "testing"

var tests = []struct {
	N string
	F func() int
	V int
}{
	{"P1", P1, 233168},
	{"P2", P2, 4613732},
	{"P3", P3, 6857},
	{"P4", P4, 906609},
	{"P5", P5, 232792560},
	{"P6", P6, 25164150},
}

func TestSolutions(t *testing.T) {
	for _, test := range tests {
		t.Run(test.N, func(t *testing.T) {
			if test.F() != test.V {
				t.Fail()
			}
		})
	}
}
