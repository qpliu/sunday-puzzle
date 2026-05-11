package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) float64 {
	g1 := r.Float64() * 12
	g2 := r.Float64() * 12
	pour := min(g1, g2)
	pitcher := 2 * pour
	g1 = max(g1-pour, g2-pour)
	g2 = r.Float64() * 12
	return pitcher + 2*min(g1, g2)
}

func simulate(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			sum := float64(0)
			for range niter {
				sum += trial(r)
			}
			ch <- sum / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(runtime.NumCPU()))
}

func ectrial(r *rand.Rand) float64 {
	g1 := r.Float64() * 12
	g2 := r.Float64() * 12
	g3 := r.Float64() * 12
	g1, g2 = max(g1, g2), min(g1, g2)
	g2, g3 = max(g2, g3), min(g2, g3)
	pitcher := 3 * g3

	g1 -= g3
	g2 -= g3
	g3 = r.Float64() * 12
	g1, g2 = max(g1, g2), min(g1, g2)
	g2, g3 = max(g2, g3), min(g2, g3)
	pitcher += 3 * g3

	g1 -= g3
	g2 -= g3
	g3 = r.Float64() * 12
	return pitcher + 3*min(g1, g2, g3)
}

func ecsimulate(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			sum := float64(0)
			for range niter {
				sum += ectrial(r)
			}
			ch <- sum / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260508
	r := rand.New(rand.NewSource(seed))

	const niter = 10000000
	simulate(niter, r)
	ecsimulate(niter, r)
}
