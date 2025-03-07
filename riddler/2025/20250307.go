package main

import (
	"fmt"
	"math/rand"
	"sort"
)

func trial(n int, r *rand.Rand) int {
	i := 0
	for {
		if r.Intn(n) == 0 {
			return i
		}
		i++
	}
}

func simulate(niters, n int, r *rand.Rand) {
	counts := make([]int, niters)
	for i := range niters {
		counts[i] = trial(n, r)
	}
	sort.Ints(counts)
	fmt.Printf("1/%d: %d %f\n", n, counts[niters/2], float64(counts[niters/2])/float64(n))
}

func main() {
	const seed = 20250307
	r := rand.New(rand.NewSource(seed))

	const niters = 1000000
	simulate(niters, 100, r)
	simulate(niters, 1000, r)
}
