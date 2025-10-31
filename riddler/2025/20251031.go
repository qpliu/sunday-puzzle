package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(n int, r *rand.Rand) bool {
	w := 1
	l := 0
	for {
		if w == n {
			return true
		} else if l == n {
			return false
		}
		if r.Intn(2) == 1 {
			w++
		} else {
			l++
		}
	}
}

func simulate(niters, ncpu, n int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range niters {
				if trial(n, r) {
					count++
				}
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}
	fmt.Printf("%d %f\n", n, float64(2*count-niters*ncpu)/float64(niters*ncpu))
}

func main() {
	const seed = 20251031
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 2000000
	for i := range 20 {
		simulate(niters, ncpu, i+1, r)
	}
}
