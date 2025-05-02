package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) int {
	a := r.Intn(12)
	b := r.Intn(11)
	if b >= a {
		a, b = b+1, a
	}
	c := r.Intn(10)
	if c >= a-1 {
		a, b, c = c+2, a, b
	} else if c >= b {
		b, c = c+1, b
	}
	nrides := 3
	for a < 11 {
		a += 1 + r.Intn(11-a)
		nrides++
	}
	return nrides
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			nrides := 0
			for _ = range niters {
				nrides += trial(r)
			}
			ch <- nrides
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	nrides := 0
	for _ = range ncpu {
		nrides += <-ch
	}
	fmt.Printf("%d/%d %f\n", nrides, niters*ncpu, float64(nrides)/float64(niters*ncpu))
}

func ectrial(r *rand.Rand) int {
	a := r.Intn(12)
	b := r.Intn(11)
	if b >= a {
		a, b = b+1, a
	}
	c := r.Intn(10)
	if c >= a-1 {
		a, b, c = c+2, a, b
	} else if c >= b {
		b, c = c+1, b
	}
	nrides := 3
	for c < 9 {
		c += 1 + r.Intn(9-c)
		if c >= a-1 {
			a, b, c = c+2, a, b
		} else if c >= b {
			b, c = c+1, b
		}
		nrides++
	}
	return nrides
}

func ecsimulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			nrides := 0
			for _ = range niters {
				nrides += ectrial(r)
			}
			ch <- nrides
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	nrides := 0
	for _ = range ncpu {
		nrides += <-ch
	}
	fmt.Printf("%d/%d %f\n", nrides, niters*ncpu, float64(nrides)/float64(niters*ncpu))
}

func main() {
	const seed = 20250502
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 1000000
	simulate(niters, ncpu, r)
	ecsimulate(niters, ncpu, r)
}
