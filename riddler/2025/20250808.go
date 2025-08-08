package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) (float64, bool) {
	a := r.Float64()
	b := r.Float64()
	f := a * b
	return f, r.Float64() < f
}

func simulate(niters, ncpu, nbuckets int, r *rand.Rand) {
	ch := make(chan []int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			buckets := make([]int, nbuckets)
			for _ = range niters {
				f, c := trial(r)
				if c {
					buckets[int(float64(nbuckets)*f)]++
				}
			}
			ch <- buckets
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	buckets := make([]int, nbuckets)
	for _ = range ncpu {
		b := <-ch
		for i := range b {
			buckets[i] += b[i]
		}
	}
	for i, c := range buckets {
		fmt.Printf("%f: %d\n", float64(i)/float64(nbuckets), c)
	}
}

func main() {
	const seed = 20250808
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 2000000
	const nbuckets = 100
	simulate(niters, ncpu, nbuckets, r)
}
