package solutions

import "github.com/tekhnus/project-euler-go"
import "math/big"

func P5() int {
	var lcm big.Int
	lcm.SetInt64(1)
	for smallx := 1; smallx <= 20; smallx++ {
		var x big.Int
		x.SetInt64(int64(smallx))
		euler.LCM(&lcm, &lcm, &x)
	}
	return int(lcm.Int64())
}
