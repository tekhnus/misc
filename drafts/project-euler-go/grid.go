package euler

type Vec2D = struct {
	I, J int
}

type V2I64 struct {
	I, J int64
}

type V3I64 struct {
	I, J, K int64
}

func GridBounds(g [][]int64) (int, int) {
	return len(g), len(g[0])
}

func GridGet(g [][]int64, s []Vec2D) ([]int64, bool) {
	maxi, maxj := GridBounds(g)
	values := make([]int64, len(s))
	for n, p := range s {
		if p.I < 0 || p.I >= maxi || p.J < 0 || p.J >= maxj {
			return nil, true
		}
		values[n] = g[p.I][p.J]
	}
	return values, false
}

func GridEach(g [][]int64, f func(Vec2D)) {
	maxi, maxj := GridBounds(g)
	for i := 0; i < maxi; i++ {
		for j := 0; j < maxj; j++ {
			f(Vec2D{i, j})
		}
	}
}

func GridSegment(p Vec2D, d Vec2D, l int) []Vec2D {
	s := make([]Vec2D, l)
	for n := 0; n < l; n++ {
		s[n] = p
		p.I += d.I
		p.J += d.J
	}
	return s
}
