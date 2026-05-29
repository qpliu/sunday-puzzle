package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func see(x1, y1, x2, y2, a float64) bool {
	delta := a - math.Atan2(y2-y1, x2-x1)
	if delta > -math.Pi/2 && delta < math.Pi/2 {
		return true
	}
	if delta > -3*math.Pi/2 && delta < 3*math.Pi/2 {
		return false
	}
	return true
}

func trial(r *rand.Rand) bool {
	x1 := r.Float64()
	y1 := r.Float64()
	a1 := r.Float64()*2*math.Pi - math.Pi
	x2 := r.Float64()
	y2 := r.Float64()
	a2 := r.Float64()*2*math.Pi - math.Pi
	return see(x1, y1, x2, y2, a1) && see(x2, y2, x1, y1, a2)
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
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("f:%f\n", sum/float64(runtime.NumCPU()))
}

func ectrial(r *rand.Rand) bool {
	x1 := r.Float64()
	y1 := r.Float64()
	a1 := r.Float64()*2*math.Pi - math.Pi
	x2 := r.Float64()
	y2 := r.Float64()
	a2 := r.Float64()*2*math.Pi - math.Pi
	x3 := r.Float64()
	y3 := r.Float64()
	a3 := r.Float64()*2*math.Pi - math.Pi
	return see(x1, y1, x2, y2, a1) && see(x1, y1, x3, y3, x1) && see(x2, y2, x1, y1, a2) && see(x2, y2, x3, y3, a2) && see(x3, y3, x1, y1, a3) && see(x3, y3, x2, y2, a3)
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
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("ec:%f\n", sum/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260529
	r := rand.New(rand.NewSource(seed))

	const niter = 20000000
	simulate(niter, r)
	ecsimulate(niter, r)
}
