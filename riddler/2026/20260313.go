package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func cantor(depth int, r *rand.Rand) float64 {
	p1 := float64(0)
	p2 := float64(1)
	for range depth {
		dp := p2 - p1
		if r.Intn(2) == 0 {
			p1 += 2 * dp / 3
		} else {
			p2 -= 2 * dp / 3
		}
	}
	if r.Intn(2) == 0 {
		return p1
	} else {
		return p2
	}
}

func trial(depth int, r *rand.Rand) float64 {
	d := cantor(depth, r) - cantor(depth, r)
	return max(d, -d)
}

func simulate(niter, depth int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			sum := float64(0)
			for range niter {
				sum += trial(depth, r)
			}
			ch <- sum / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("fiddler: %d,%d: %f\n", niter, depth, sum/float64(runtime.NumCPU()))
}

func ectrial(depth int, r *rand.Rand) bool {
	p1, p2, p3 := cantor(depth, r), cantor(depth, r), cantor(depth, r)
	if p1 > p2 {
		p1, p2 = p2, p1
	}
	if p2 > p3 {
		p2, p3 = p3, p2
	}
	return p1+p2 > p3
}

func ecsimulate(niter, depth int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			count := 0
			for range niter {
				if ectrial(depth, r) {
					count++
				}
			}
			ch <- float64(count) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("extra credit: %d,%d: %f\n", niter, depth, sum/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260313
	r := rand.New(rand.NewSource(seed))
	for _, depth := range [...]int{5, 10, 15} {
		for _, niter := range [...]int{10000, 100000, 1000000, 10000000} {
			simulate(niter, depth, r)
		}
	}
	for _, depth := range [...]int{5, 10, 15} {
		for _, niter := range [...]int{10000, 100000, 1000000, 10000000} {
			ecsimulate(niter, depth, r)
		}
	}
}
