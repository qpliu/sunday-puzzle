package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

var winners = []int{
	1<<3 | 1<<4 | 1<<5,
	1<<6 | 1<<7 | 1<<8,
	1<<9 | 1<<10 | 1<<11,
	1<<3 | 1<<6 | 1<<9,
	1<<4 | 1<<7 | 1<<10,
	1<<5 | 1<<8 | 1<<11,
	1<<3 | 1<<7 | 1<<11,
	1<<5 | 1<<7 | 1<<9,
}

func win(grid int) bool {
	for _, w := range winners {
		if w&grid == w {
			return true
		}
	}
	return false
}

func trial(maxRolls int, r *rand.Rand) int {
	grid := 0
	for i := range maxRolls {
		grid |= 1 << (2 + r.Intn(6) + r.Intn(6))
		if win(grid) {
			return i + 1
		}
	}
	return 0
}

func simulate(niters, ncpu, maxRolls int, r *rand.Rand) {
	ch := make(chan []int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			counts := make([]int, maxRolls+1)
			for _ = range niters {
				nrolls := trial(maxRolls, r)
				if nrolls > 0 {
					for i := nrolls; i <= maxRolls; i++ {
						counts[i]++
					}
				}
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	counts := make([]int, maxRolls+1)
	for _ = range ncpu {
		c := <-ch
		for i := range counts {
			counts[i] += c[i]
		}
	}
	for i, n := range counts {
		fmt.Printf("%d:%f\n", i, float64(n)/float64(niters*ncpu))
	}
}

func main() {
	const seed = 20251010
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 1000000
	const maxRolls = 15
	simulate(niters, ncpu, maxRolls, r)
}
