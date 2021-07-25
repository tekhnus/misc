package solutions

import "testing"
import "reflect"

var tests = []struct {
	N string
	F interface{}
	V interface{}
}{
	{"P1", P1, 233168},
	{"P2", P2, 4613732},
	{"P3", P3, 6857},
	{"P4", P4, 906609},
	{"P5", P5, 232792560},
	{"P6", P6, 25164150},
	{"P7", P7, 104743},
	{"P8", P8, 23514624000},
	{"P9", P9, 31875000},
	{"P10", P10, 142913828922},
	{"P11", P11, 70600674},
	{"P12", P12, 76576500},
	{"P13", P13, "5537376230"},
}

func TestSolutions(t *testing.T) {
	for _, test := range tests {
		t.Run(test.N, func(t *testing.T) {
			if !reflect.DeepEqual(reflect.ValueOf(test.F).Call([]reflect.Value{})[0].Interface(), test.V) {
				t.Fail()
			}
		})
	}
}
