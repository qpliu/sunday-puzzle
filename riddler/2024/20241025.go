package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func simulate(n, ncpu, cornMax, cups, draws int, r *rand.Rand) {
	ch := make(chan [2]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			counts := [2]int{0, 0}
			for _ = range n {
				corn := r.Intn(cornMax)
				cupsDrawn := 0
				for _ = range draws {
					if r.Intn(corn+cups-cupsDrawn) < cups-cupsDrawn {
						cupsDrawn++
					} else {
						break
					}
				}
				if cupsDrawn == draws {
					counts[0] += corn
					counts[1] += 1
				}
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	counts := [2]int{0, 0}
	for _ = range ncpu {
		c := <-ch
		counts[0] += c[0]
		counts[1] += c[1]
	}
	fmt.Printf("M=%d N=%d k=%d %d/%d", cornMax, cups, draws, counts[0], counts[1])
	if counts[1] > 0 {
		fmt.Printf(" %f", float64(counts[0])/float64(counts[1]))
	}
	fmt.Printf(" (%f)\n", float64(cups-draws+1)/float64(draws-2))
}

func main() {
	const seed = 20241025
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	for cups := 3; cups < 8; cups++ {
		for draws := 3; draws <= cups; draws++ {
			const n = 10000000
			const cornMax = 50
			simulate(n, ncpu, cornMax, cups, draws, r)
			if draws == 3 {
				simulate(n, ncpu, 2*cornMax, cups, draws, r)
				simulate(n, ncpu, 3*cornMax, cups, draws, r)
			}
		}
	}
}
