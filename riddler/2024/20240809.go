package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
	"sync"
)

func trial(da, db float64, r *rand.Rand) bool {
	xa := r.Float64()*10
	xb := r.Float64()*10
	return math.Signbit(da+xa - (db+xb)) == math.Signbit(da*xa - (db*xb))
}

func simulate(n, ncpu int, r *rand.Rand) int {
	const da = 6
	const db = 5
	counts := make([]int, ncpu)
	var wg sync.WaitGroup
	wg.Add(ncpu)
	for i := range ncpu {
		go func(icpu int, r *rand.Rand) {
			for _ = range n {
				if trial(da, db, r) {
					counts[icpu]++
				}
			}
			wg.Done()
		}(i, rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	wg.Wait()
	count := 0
	for _, c := range counts {
		count += c
	}
	return count
}

func ecsimulate(n, ncpu int, r *rand.Rand) int {
	counts := make([]int, ncpu)
	var wg sync.WaitGroup
	wg.Add(ncpu)
	for i := range ncpu {
		go func(icpu int, r *rand.Rand) {
			for _ = range n {
				da := r.Float64()*10
				db := r.Float64()*10
				if trial(da, db, r) {
					counts[icpu]++
				}
			}
			wg.Done()
		}(i, rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	wg.Wait()
	count := 0
	for _, c := range counts {
		count += c
	}
	return count
}

func main() {
	const seed = 1673048
	r := rand.New(rand.NewSource(seed))
	const n = 20000000
	ncpu := runtime.NumCPU()
	{
		m := simulate(n, ncpu, r)
		fmt.Printf("%d/%d %f\n", m, n*ncpu, float64(m)/float64(n*ncpu))
	}
	{
		m := ecsimulate(n, ncpu, r)
		fmt.Printf("%d/%d %f\n", m, n*ncpu, float64(m)/float64(n*ncpu))
	}
}
