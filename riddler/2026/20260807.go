package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"slices"
)

func trial(ignoreTies bool, r *rand.Rand) int {
	for {
		g := 0
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

func ectrial(wins []int, r *rand.Rand) {
	clear(wins)
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
	slices.Sort(wins)
}

func ecsimulate(niter, nteams int, r *rand.Rand) {
	ch := make(chan []float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			total := make([]float64, nteams)
			wins := make([]int, nteams)
			for range niter {
				ectrial(wins, r)
				for i, w := range wins {
					total[i] += float64(w)
				}
			}
			for i := range total {
				total[i] /= float64(niter)
			}
			ch <- total
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := make([]float64, nteams)
	for range runtime.NumCPU() {
		for i, w := range <-ch {
			sums[i] += w
		}
	}
	fmt.Printf("w_avg[%d] = [", nteams)
	sep := ""
	for _, sum := range sums {
		fmt.Printf("%s%f", sep, sum/float64(runtime.NumCPU()))
		sep = ","
	}
	fmt.Printf("]\n")
}

func main() {
	const seed = 20260807
	r := rand.New(rand.NewSource(seed))

	const niter = 500000
	simulate(false, niter, r)
	simulate(true, niter, r)
	for i := range 29 {
		nteams := i + 2
		ecsimulate(niter, nteams, r)
	}
}
