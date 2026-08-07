package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(ignoreTies bool, r *rand.Rand) int {
	g := 0
	for {
		for range 162 {
			if r.Intn(2) == 0 {
				g++
			}
		}
		if ignoreTies && g == 81 {
			continue
		}
		return max(g, 162-g)
	}
}

func simulate(ignoreTies bool, niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			total := 0
			for range niter {
				total += trial(ignoreTies, r)
			}
			ch <- float64(total) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(runtime.NumCPU()))
}

func ectrial(r *rand.Rand) int {
	wins := [30]int{}
	for i := range wins {
		for j := range wins[i+1:] {
			for range 5 {
				if r.Intn(2) == 0 {
					wins[i]++
				} else {
					wins[i+1+j]++
				}
			}
		}
	}
	m := 0
	for _, w := range wins {
		m = max(m, w)
	}
	return m
}

func ecsimulate(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			total := 0
			for range niter {
				total += ectrial(r)
			}
			ch <- float64(total) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260807
	r := rand.New(rand.NewSource(seed))

	const niter = 500000
	simulate(false, niter, r)
	simulate(true, niter, r)
	ecsimulate(niter, r)
}
