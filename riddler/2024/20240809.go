package main

import (
	"fmt"
	"math"
	"math/rand"
)

func trial(da, db float64, r *rand.Rand) bool {
	xa := r.Float64()*10
	xb := r.Float64()*10
	return math.Signbit(da+xa - (db+xb)) == math.Signbit(da*xa - (db*xb))
}

func simulate(n int, r *rand.Rand) int {
	count := 0
	const da = 6
	const db = 5
	for _ = range n {
		if trial(da, db, r) {
			count++
		}
	}
	return count
}

func ecsimulate(n int, r *rand.Rand) int {
	count := 0
	for _ = range n {
		da := r.Float64()*10
		db := r.Float64()*10
		if trial(da, db, r) {
			count++
		}
	}
	return count
}

func main() {
	const seed = 1673048
	r := rand.New(rand.NewSource(seed))
	const n = 10000000
	{
		m := simulate(n, r)
		fmt.Printf("%d/%d %f\n", m, n, float64(m)/float64(n))
	}
	{
		m := ecsimulate(n, r)
		fmt.Printf("%d/%d %f\n", m, n, float64(m)/float64(n))
	}
}
