package euler

import "io"
import "fmt"

func FscanlnDecimals64(r io.Reader, ns []int64) error {
	for j, _ := range ns {
		_, err := fmt.Fscanf(r, "%d", &ns[j])
		if err != nil {
			return err
		}
	}
	_, err := fmt.Fscanf(r, "\n")
	return err
}
