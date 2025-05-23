package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func hasSpace(n int, r *rand.Rand) bool {
	i := 0
	for i < n {
		if r.Intn(2) == 1 {
			i += 4
		} else {
			i += 5
		}
	}
	return i == n
}

func trial(r *rand.Rand) int {
	l := 1
	for hasSpace(l+12, r) {
		l++
	}
	return l
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			nrows := 0
			for _ = range niters {
				nrows += trial(r)
			}
			ch <- nrows
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	nrows := 0
	for _ = range ncpu {
		nrows += <-ch
	}
	fmt.Printf("%d/%d %f\n", nrows, niters*ncpu, float64(nrows)/float64(niters*ncpu))
}

func main() {
	const seed = 20250523
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 1000000
	simulate(niters, ncpu, r)
}
