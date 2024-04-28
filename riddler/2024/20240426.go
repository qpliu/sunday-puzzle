package main

import (
	"fmt"
	"math/rand"
)

func trial(x float64, r *rand.Rand) bool {
	a := r.Float64()
	if a < x {
		a += r.Float64()
		if a > 1 {
			return false
		}
	}
	b := r.Float64()
	if b > a {
		return false
	}
	b += r.Float64()
	if b > a && b < 1 {
		return false
	}
	return true
}

func simulate(x float64, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if trial(x, r) {
			wins++
		}
	}
	return wins
}

func ectrial(x, y float64, r *rand.Rand) bool {
	a := r.Float64()
	if a < y {
		a += r.Float64()
		if a > 1 {
			return false
		}
	}
	b := r.Float64()
	if b > a {
		return false
	}
	if b < a || b < x {
		b += r.Float64()
		if b > a && b < 1 {
			return false
		}
	}
	c := r.Float64()
	if c > a {
		return false
	}
	c += r.Float64()
	if c > a && c < 1 {
		return false
	}
	return true
}

func ecsimulate(x, y float64, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if ectrial(x, y, r) {
			wins++
		}
	}
	return wins
}

func fttrial(x float64, r *rand.Rand) bool {
	a := r.Float64()
	for a < x {
		a += r.Float64()
	}
	if a > 1 {
		return false
	}
	b := r.Float64()
	for b < a {
		b += r.Float64()
	}
	if b > a && b < 1 {
		return false
	}
	return true
}

func ftsimulate(x float64, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if fttrial(x, r) {
			wins++
		}
	}
	return wins
}

func main() {
	const n = 1000000
	const seed = 806112731
	r := rand.New(rand.NewSource(seed))
	xs := []float64{0.50,0.5321,0.55,0.5701,0.60,0.65}
	for _, x := range xs {
		m := simulate(x, n, r)
		fmt.Printf("x=%f %d/%d %f\n", x, m, n, float64(m)/n)
	}
	ys := []float64{0.60,0.64865,0.65,0.70}
	for _, x := range xs {
		for _, y := range ys {
			m := ecsimulate(x, y, n, r)
			fmt.Printf("x=%f y=%f %d/%d %f\n", x, y, m, n, float64(m)/n)
		}
	}
	for _, x := range xs {
		m := ftsimulate(x, n, r)
		fmt.Printf("x=%f %d/%d %f\n", x, m, n, float64(m)/n)
	}
}
