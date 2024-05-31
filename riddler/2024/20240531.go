package main

import (
	"fmt"
	"math/rand"
)

func trial(depth int, r *rand.Rand) float64 {
	s := make([][2]float64, 1<<depth)
	for i := range s {
		length := float64(1)
		split := r.Float64()
		if i != 0 {
			length = s[(i-1)/2][i%2]
		}
		s[i] = [2]float64{length*split,length*(1-split)}
	}
	sum := float64(0)
	for _, item := range s {
		sum += item[0]*item[1]
	}
	return sum
}

func simulate(n, depth int, r *rand.Rand) float64 {
	sum := float64(0)
	for i := 0; i < n; i++ {
		sum += trial(depth, r)
	}
	return sum/float64(n)
}

func main() {
	const n = 100000
	const seed = 7406
	r := rand.New(rand.NewSource(seed))
	for _, depth := range []int{10,11,12,13} {
		fmt.Printf("%d %f\n", depth, simulate(n, depth, r))
	}
}
