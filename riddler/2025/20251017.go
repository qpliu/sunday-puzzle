package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) float64 {
	theta := r.Float64() * math.Pi / 2
	st, ct := math.Sincos(theta)
	rx := 1 / (2 * ct)
	ry := 1 / (2 * st)
	return min(rx, ry)
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan float64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sum := float64(0)
			for _ = range niters {
				sum += trial(r)
			}
			ch <- sum / float64(niters)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for _ = range ncpu {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(ncpu))
}

func ectrial(r *rand.Rand) float64 {
	phi := r.Float64() * math.Pi / 2
	theta := r.Float64() * math.Pi / 2
	sp, cp := math.Sincos(phi)
	st, ct := math.Sincos(theta)
	rx := 1 / (2 * sp * ct)
	ry := 1 / (2 * sp * st)
	rz := 1 / (2 * cp)
	return min(rx, ry, rz)
}

func ecsimulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan float64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sum := float64(0)
			for _ = range niters {
				sum += ectrial(r)
			}
			ch <- sum / float64(niters)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for _ = range ncpu {
		sum += <-ch
	}
	fmt.Printf("%f\n", sum/float64(ncpu))
}

func main() {
	const seed = 20251017
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 10000000
	simulate(niters, ncpu, r)
	ecsimulate(niters, ncpu, r)
}
