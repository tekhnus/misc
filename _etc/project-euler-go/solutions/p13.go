package solutions

import _ "embed"
import "strings"
import "math/big"
import "fmt"

//import "github.com/tekhnus/project-euler-go"

//go:embed p13.txt
var p13txt string

func P13() string {
	stream := strings.NewReader(p13txt)
	var s big.Int
	for i := 0; i < 100; i++ {
		var line string
		fmt.Fscanln(stream, &line)
		var x big.Int
		x.SetString(line, 10)
		s.Add(&s, &x)
	}
	return s.String()[:10]
}
