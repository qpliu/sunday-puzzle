package main

import (
	"fmt"
	"math/rand"
)

func sim(n, npairs int) []float64 {
	counts := make([]int, npairs+1)
	hat := make([]int, 2*npairs)
	swap := func(i, j int) {
		hat[i], hat[j] = hat[j], hat[i]
	}
	for i := 0; i < n; i++ {
		for j := range hat {
			hat[j] = rand.Intn(2*npairs - 1)
			if hat[j] >= j {
				hat[j]++
			}
		}
		rand.Shuffle(2*npairs, swap)
		matches := 0
		for j, k := range hat {
			if j != k && j == hat[k] {
				matches++
			}
		}
		if matches%2 != 0 {
			panic(fmt.Sprintf("matches=%d, hat=%d", matches, hat))
		}
		counts[matches/2]++
	}
	coeffs := make([]float64, npairs+1)
	for i, count := range counts {
		coeffs[i] = float64(count)/float64(n)
	}
	return coeffs
}

func main() {
	const maxPairs = 10
	r := make([]float64, maxPairs+1)
	for npairs := 1; npairs <= maxPairs; npairs++ {
		n := 2000000 + 1000000*npairs
		coeffs := sim(n, npairs)

		num := float64(1)
		for i := 1; i < npairs; i++ {
			num += r[npairs - i]*coeffs[i];
		}
		r[npairs] = num/(1-coeffs[0])
		fmt.Printf("n=%d, npairs=%d, r=%g, coeffs=%g\n", n, npairs, r[npairs], coeffs)
	}
}
