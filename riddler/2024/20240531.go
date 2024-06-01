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

func ectrial(depth int, l float64, r *rand.Rand) float64 {
	if depth <= 0 {
		return l*l*l/6
	}
	a := r.Float64()
	b := r.Float64()
	c := r.Float64()
	for a+b+c == 0 {
		a = r.Float64()
		b = r.Float64()
		c = r.Float64()
	}
	a, b, c = l*a/(a+b+c), l*b/(a+b+c), l*c/(a+b+c)
	result := a*b*c + ectrial(depth-1, a+b, r) + ectrial(depth-1, a+c, r) + ectrial(depth-1, b+c, r) - ectrial(depth-1, a, r) - ectrial(depth-1, b, r) - ectrial(depth-1, c, r)
	// fmt.Printf("%d %f %f %f %f %f %f %f\n", depth, a, b, c, l, a*b*c, result, l*l*l/6)
	return result
}

func ecsimulate(n, depth int, r *rand.Rand) float64 {
	sum := float64(0)
	for i := 0; i < n; i++ {
		sum += ectrial(depth, 1, r)
	}
	return sum/float64(n)
}

func main() {
	const n = 10000
	const seed = 74068
	r := rand.New(rand.NewSource(seed))
	for _, depth := range []int{10,11,12,13} {
		fmt.Printf("%d %f\n", depth, simulate(n, depth, r))
	}
	const nn = 1
	for _, depth := range []int{1,2,3,4,5,6} {
		fmt.Printf("%d %f\n", depth, ecsimulate(nn, depth, r))
	}
}
