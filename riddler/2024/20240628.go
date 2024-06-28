package main

import (
	"fmt"
	"math"
	"math/rand"
)

func trial(np int, r *rand.Rand) int {
	socks := make([]int, 2*np)
	for i := range socks {
		socks[i] = i % np
	}
	rand.Shuffle(2*np, func(i, j int) {
		socks[i], socks[j] = socks[j], socks[i]
	})
	set := make(map[int]bool)
	for i := range socks {
		if set[socks[i]] {
			return i
		}
		set[socks[i]] = true
	}
	panic("no pairs")
}

func simulate(n, np int, r *rand.Rand) []int {
	counts := make([]int, np+1)
	for _ = range n {
		counts[trial(np, r)]++
	}
	return counts
}

func pp(np, n int) float64 {
	result := 1.0
	for i := range n {
		result *= 2*float64(np-i)/float64(2*np-i)
	}
	return result*float64(n)/float64(2*np-n)
}

func main() {
	const n = 200000
	const seed = 7406833
	r := rand.New(rand.NewSource(seed))
	run := func(np int) {
		counts := simulate(n, np, r)
		maxi := 0
		for i := range counts {
			if counts[i] > counts[maxi] {
				maxi = i
			}
		}
		fmt.Printf("np=%d %d %f\n", np, maxi+1, math.Sqrt(2*float64(np)))
		for i := max(0, maxi-3); i < min(len(counts), maxi+3); i++ {
			fmt.Printf("  %d %d/%d %f %f\n", i, counts[i], n, float64(counts[i])/n, pp(np, i))
		}
	}
	run(5)
	run(10)
	run(50)
	run(100)
	run(500)
	run(1000)
}
