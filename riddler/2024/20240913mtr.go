package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(buckets []int, r *rand.Rand) {
	z := math.Exp(math.Log(r.Float64()*r.Float64())*r.Float64())
	buckets[int(z*float64(len(buckets)))]++
}

func simulate(n, nbuckets, ncpu int, r *rand.Rand) []int {
	ch := make(chan []int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			buckets := make([]int, nbuckets)
			for _ = range n {
				trial(buckets, r)
			}
			ch <- buckets
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	buckets := make([]int, nbuckets)
	for _ = range ncpu {
		b := <- ch
		for i := range buckets {
			buckets[i] += b[i]
		}
	}
	return buckets
}

func main() {
	const seed = 20240913
	r := rand.New(rand.NewSource(seed))
	const nbuckets = 20
	const n = nbuckets*10000
	ncpu := runtime.NumCPU()
	buckets := simulate(n, nbuckets, ncpu, r)
	for i, x := range(buckets) {
		fmt.Printf("%f: %f\n", float64(i)/float64(nbuckets), float64(x)/float64(ncpu*n))
	}
}
