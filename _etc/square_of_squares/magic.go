/*
-- Checks all magic squares with square numbers in center and corners.
-- Some non-primitive arithmetic progressions are not checked, so the search is non-exaustive.
-- The parameter values up to 10^13 were checked (so middle cell values up to 10^26 were inspected).
-- No squares with seven square numbers were found :'(
*/
package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"log"
	"math"
	"math/big"
	"os"
)

type Output struct {
	SqCnt int
	A11   string
	D1    string
	D2    string
}

func findSquares(from, to int64) {
	d := map[int64][]struct{ m, n int64 }{}

	paramFrom := int64(math.Sqrt(math.Ceil(float64(from) / 2)))
	paramTo := int64(math.Ceil(math.Sqrt(float64(to))))
	// println("Precalc starting for", from, to)
	for m := paramFrom; m < paramTo; m++ {
		m2 := m * m
		var nFrom int64
		if m2 < from {
			nFrom = int64(math.Floor(math.Sqrt(float64(from - m2))))
		} else {
			nFrom = 1
		}
		nTo := 1 + int64(math.Floor(math.Sqrt(float64(to-m2))))
		if nTo >= m {
			nTo = m
		}
		for n := nFrom; n < nTo; n++ {
			su := m2 + n*n
			if su < from || su >= to {
				continue
			}
			d[su] = append(d[su], struct{ m, n int64 }{m, n})
		}
	}
	// println("Calc starting for", from, to)
	for su, mns := range d {
		var a11 big.Int
		a11.SetInt64(su)
		a11.Mul(&a11, &a11)
		for i, mn1 := range mns[1:] {
			var m1, n1, d1 big.Int
			m1.SetInt64(mn1.m)
			n1.SetInt64(mn1.n)
			var m1n1sqrDiff big.Int
			m1n1sqrDiff.Mul(new(big.Int).Add(&m1, &n1), new(big.Int).Sub(&m1, &n1))
			d1.SetInt64(4).Mul(&d1, &m1).Mul(&d1, &n1).Mul(&d1, &m1n1sqrDiff)
			for _, mn2 := range mns[:i+1] {
				var m2, n2, d2 big.Int
				m2.SetInt64(mn2.m)
				n2.SetInt64(mn2.n)
				var m2n2sqrDiff big.Int
				m2n2sqrDiff.Mul(new(big.Int).Add(&m2, &n2), new(big.Int).Sub(&m2, &n2))
				d2.SetInt64(4).Mul(&d2, &m2).Mul(&d2, &n2).Mul(&d2, &m2n2sqrDiff)
				var d1plusd2 big.Int
				d1plusd2.Add(&d1, &d2)
				var d1minusd2 big.Int
				d1minusd2.Sub(&d1, &d2)
				var a01, a10, a12, a21 big.Int
				a01.Add(&a11, &d1plusd2)
				a10.Add(&a11, &d1minusd2)
				a12.Sub(&a11, &d1minusd2)
				a21.Sub(&a11, &d1plusd2)
				sqCnt := 5
				var tmp big.Int
				if a01.Sign() >= 0 && tmp.Sqrt(&a01).Mul(&tmp, &tmp).Cmp(&a01) == 0 {
					sqCnt += 1
				}
				if a10.Sign() >= 0 && tmp.Sqrt(&a10).Mul(&tmp, &tmp).Cmp(&a10) == 0 {
					sqCnt += 1
				}
				if a12.Sign() >= 0 && tmp.Sqrt(&a12).Mul(&tmp, &tmp).Cmp(&a12) == 0 {
					sqCnt += 1
				}
				if a21.Sign() >= 0 && tmp.Sqrt(&a21).Mul(&tmp, &tmp).Cmp(&a21) == 0 {
					sqCnt += 1
				}
				if sqCnt > 5 {
					outp := Output{sqCnt, a11.String(), d1.String(), d2.String()}
					v, _ := json.Marshal(outp)
					fmt.Println(string(v))
				}
			}
		}
	}
	// println("Finished for", from, to)
}

type Message struct {
	From int64
	To   int64
}

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	for scanner.Scan() {
		var m Message
		err := json.Unmarshal(scanner.Bytes(), &m)
		if err != nil {
			log.Fatal(err)
		}
		findSquares(m.From, m.To)
	}
}
