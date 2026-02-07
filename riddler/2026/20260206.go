package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func game(a, b *int, r *rand.Rand) {
	if r.Intn(2) == 0 {
		*a = 0
		*b++
	} else {
		*a++
		*b = 0
	}
}

func simulate(nskip, niters int, r *rand.Rand) {
	out := make(chan int)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			a, b := 0, 0
			for range nskip {
				game(&a, &b, r)
			}
			sum := 0
			for range niters {
				game(&a, &b, r)
				sum += max(a, b)
			}
			out <- sum
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := 0
	for range runtime.NumCPU() {
		sum += <-out
	}
	fmt.Printf("fiddler:%f\n", float64(sum)/float64(niters*runtime.NumCPU()))
}

func ecsimulate(nskip, niters int, r *rand.Rand) {
	out := make(chan int)
	outDist := make(chan map[[3]int]int)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			a, b, c := 0, 0, 0
			for range nskip {
				game(&a, &b, r)
				game(&b, &c, r)
				game(&c, &a, r)
			}
			sum := 0
			dist := map[[3]int]int{}
			for range niters {
				dist[[3]int{b, a, c}] += 1
				game(&a, &b, r)
				sum += max(a, b, c)
				dist[[3]int{c, b, a}] += 1
				game(&b, &c, r)
				sum += max(a, b, c)
				dist[[3]int{a, c, b}] += 1
				game(&c, &a, r)
				sum += max(a, b, c)
			}
			out <- sum
			outDist <- dist
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := 0
	for range runtime.NumCPU() {
		sum += <-out
	}
	fmt.Printf("extra credit:%f\n", float64(sum)/float64(3*niters*runtime.NumCPU()))
	dist := map[[3]int]float64{}
	for range runtime.NumCPU() {
		for s, n := range <-outDist {
			dist[s] += float64(n) / float64(3*niters*runtime.NumCPU())
		}
	}
	fmt.Printf("extra credit distribution: %v\n", dist)
}

func main() {
	const seed = 20260206
	r := rand.New(rand.NewSource(seed))

	const nskip = 100
	const niters = 10000000
	simulate(nskip, niters, r)
	ecsimulate(nskip, niters, r)
}
