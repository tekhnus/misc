package euler

import "sync"
import "reflect"

func WriteIntegers(c chan int64, from int64, to int64) {
	for i := from; i < to; i++ {
		c <- i
	}
	close(c)
}

func WritePairs(c chan V2I64, ifrom int64, ito int64, jfrom int64, jto int64) {
	for i := ifrom; i < ito; i++ {
		for j := jfrom; j < jto; j++ {
			c <- V2I64{i, j}
		}
	}
	close(c)
}

func WriteSliceI64(c chan int64, s []int64) {
	for _, i := range s {
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

func ReadSliceI64(c chan int64) []int64 {
	var res []int64
	for x := range c {
		res = append(res, x)
	}
	return res
}

func Closer(c interface{}, wg *sync.WaitGroup) {
	wg.Wait()
	reflect.ValueOf(c).Close()
}
