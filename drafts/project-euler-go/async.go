package euler

import "sync"

func RunMany(n int, wg *sync.WaitGroup, f func()) {
	wg.Add(n)
	for i := 0; i < n; i++ {
		go func() {
			defer wg.Done()
			f()
		}()
	}
}
