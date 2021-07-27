package euler

import "sync"
import "reflect"

func WriteIntegers(c chan int64, from int64, to int64) {
	for i := from; i < to; i++ {
		c <- i
	}
	close(c)
}

func ReadMax(c chan int) int {
	max := <-c
	for v := range c {
		if v > max {
			max = v
		}
	}
	return max
}

func ReadMaxGeneric(inp chan Comparable) Comparable {
	result := <-inp
	for p := range inp {
		if p.CompareTo(result) == 1 {
			result = p
		}
	}
	return result
}

func ReadCount(c chan int) int {
	count := 0
	for _ = range c {
		count++
	}
	return count
}

func ReadSum(c chan int) int {
	sum := 0
	for x := range c {
		sum += x
	}
	return sum
}

func Closer(c interface{}, wg *sync.WaitGroup) {
	wg.Wait()
	reflect.ValueOf(c).Close()
}
