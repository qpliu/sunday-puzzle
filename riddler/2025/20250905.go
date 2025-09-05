package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) bool {
	x1 := r.Float64()
	p := 0
	for {
		x2 := r.Float64()
		if x1 > 0.5 && x1 < x2 {
			break
		} else if x1 < 0.5 && x1 > x2 {
			break
		}
		p++
		if p >= 2 {
			break
		}
		x1 = x2
	}
	return p >= 2
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range niters {
				if trial(r) {
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
	fmt.Printf("%d/%d=%f\n", count, niters*ncpu, float64(count)/float64(niters*ncpu))
}

func main() {
	const seed = 20250905
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 10000000
	simulate(niters, ncpu, r)
}
