package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

const (
	gap = 0.125
)

func ring(f float64, r *rand.Rand) float64 {
	if f == 0 {
		return 0
	}
	a := r.Float64()
	if a < f {
		return f - a
	} else if a < 1-gap {
		return 0
	} else if a < 1+f-gap {
		return a + gap - 1
	} else {
		return f
	}
}

func trial(r *rand.Rand) bool {
	f := float64(gap)
	f = ring(f, r)
	f = ring(f, r)
	return f > 0
}

func simulate(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			count := 0
			for range niter {
				if trial(r) {
					count++
				}
			}
			ch <- float64(count) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	p := float64(0)
	for range runtime.NumCPU() {
		p += <-ch
	}
	p /= float64(runtime.NumCPU())
	fmt.Printf("%f\n", p)
}

func ectrial(r *rand.Rand) bool {
	a := r.Float64()
	amax := 2 * (a + gap)
	amin := gap + 2*(a-gap)
	f := amax - amin
	if f >= 1-gap {
		return true
	}
	return ring(f, r) > 0
}

func ecsimulate(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			count := 0
			for range niter {
				if ectrial(r) {
					count++
				}
			}
			ch <- float64(count) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	p := float64(0)
	for range runtime.NumCPU() {
		p += <-ch
	}
	p /= float64(runtime.NumCPU())
	fmt.Printf("%f\n", p)
}

func main() {
	const seed = 20260731
	r := rand.New(rand.NewSource(seed))

	const niter = 10000000
	simulate(niter, r)
	ecsimulate(niter, r)
}
