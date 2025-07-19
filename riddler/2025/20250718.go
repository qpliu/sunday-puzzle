package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) int {
	a1 := r.Float64()
	a2 := r.Float64()
	a3 := r.Float64()
	if math.Abs(a1-a2) > 0.25 {
		if math.Abs(a1-a3) <= 0.25 {
			return 2
		} else if math.Abs(a2-a3) <= 0.25 {
			return 2
		} else {
			return 1
		}
	} else if math.Abs(a1-a3) > 0.25 {
		return 2
	} else if math.Abs(a2-a3) > 0.25 {
		return 2
	} else {
		return 3
	}
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan [3]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			var counts [3]int
			for _ = range niters {
				counts[trial(r)-1]++
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	var counts [3]int
	for _ = range ncpu {
		c := <-ch
		for i := range counts {
			counts[i] += c[i]
		}
	}
	sum := 0
	for _, count := range counts {
		sum += count

	}
	nsum := 0
	for i, count := range counts {
		nsum += (i + 1) * count
	}
	for i := range counts {
		fmt.Printf("%d:%d/%d %f\n", i+1, counts[i], sum, float64(counts[i])/float64(sum))
	}
	fmt.Printf("%f\n", float64(nsum)/float64(sum))
}

func ectrial(r *rand.Rand) int {
	a1 := r.Float64()
	a2 := r.Float64()
	if a2 < a1 {
		a1, a2 = a2, a1
	}
	a3 := r.Float64()
	if a3 < a1 {
		a1, a2, a3 = a3, a1, a2
	} else if a3 < a2 {
		a2, a3 = a3, a2
	}
	a4 := r.Float64()
	if a4 < a1 {
		a1, a2, a3, a4 = a4, a1, a2, a3
	} else if a4 < a2 {
		a2, a3, a4 = a4, a2, a3
	} else if a4 < a3 {
		a3, a4 = a4, a3
	}
	if a4-a1 <= 0.25 {
		return 3
	} else if a3-a1 <= 0.25 || a4-a2 <= 0.25 {
		return 2
	}
	if a2-a1 <= 0.25 || a3-a2 <= 0.25 || a4-a3 <= 0.25 {
		return 1
	}
	return 0
}

func ecsimulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan [4]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			var counts [4]int
			for _ = range niters {
				counts[ectrial(r)]++
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	var counts [4]int
	for _ = range ncpu {
		c := <-ch
		for i := range counts {
			counts[i] += c[i]
		}
	}
	sum := 0
	for _, count := range counts {
		sum += count
	}
	nsum := 0
	for i, count := range counts {
		nsum += (i + 1) * count
	}
	for i := range counts {
		fmt.Printf("%d:%d/%d %f\n", i+1, counts[i], sum, float64(counts[i])/float64(sum))
	}
	fmt.Printf("%f\n", float64(nsum)/float64(sum))
}

func main() {
	const seed = 20250718
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 1000000
	simulate(niters, ncpu, r)
	ecsimulate(niters, ncpu, r)
}
