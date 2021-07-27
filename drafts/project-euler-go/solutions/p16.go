package solutions

import "math/big"
import "github.com/tekhnus/project-euler-go"

func P16() int64 {
	var two big.Int
	two.SetInt64(2)
	var thousand big.Int
	thousand.SetInt64(1000)
	var x big.Int
	x.Exp(&two, &thousand, nil)
	return euler.Sum(euler.Digits(x))
}
