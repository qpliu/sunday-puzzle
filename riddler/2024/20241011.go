package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func simulate(n, ncpu int, r *rand.Rand) {
	ch := make(chan [2]float64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sums := [2]float64{0, 0}
			for _ = range n {
				t := r.Float64()
				p := r.Float64()
				w := p * t
				l := w - w*math.Log(w)
				sums[0] += w
				sums[1] += l
			}
			ch <- sums
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := [2]float64{0, 0}
	for _ = range ncpu {
		s := <-ch
		sums[0] += s[0]
		sums[1] += s[1]
	}
	fmt.Printf("<W>=%f\n", sums[0]/float64(n*ncpu))
	fmt.Printf("<L>=%f\n", sums[1]/float64(n*ncpu))
}

func main() {
	const seed = 20241011
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const n = 10000000
	simulate(n, ncpu, r)
}
