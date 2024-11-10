package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(position int, r *rand.Rand) bool {
	house := 0
	for _ = range position - 2 {
		request := r.Intn(4)
		if request == house {
			request := r.Intn(3)
			if request < house {
				house = request
			} else {
				house = request + 1
			}
		} else {
			house = request
		}
	}
	return house != 0
}

func simulate(n, ncpu, position int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range n {
				if trial(position, r) {
					count++
				}
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}

	p := 0.75
	threes := 1
	for _ = range position - 2 {
		threes *= 3
	}
	if position%2 == 0 {
		p -= 0.75 / float64(threes)
	} else {
		p += 0.75 / float64(threes)
	}
	fmt.Printf("%d %f %f\n", position, float64(count)/float64(n*ncpu), p)
}

func ecsimulate(n, ncpu, maxPosition int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range n {
				if trial(2+r.Intn(maxPosition-1), r) {
					count++
				}
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}

	p := float64(1640)/float64(2187)
	pp := float64(0)
	nn := 2
	threes := 1
	for _ = range maxPosition - 2 {
		pp = (float64(nn-1)*pp + 0.75 + 0.25/float64(threes)) / float64(nn)
		nn++
		threes *= -3
	}
	fmt.Printf("%d %f %f %f\n", maxPosition, float64(count)/float64(n*ncpu), pp, p)
}

func main() {
	const seed = 20241108
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	for _, position := range []int{2, 3, 4, 5, 6, 7, 8, 9, 10, 11} {
		const n = 2000000
		simulate(n, ncpu, position, r)
	}

	for _, maxPosition := range []int{2, 3, 4, 5, 6, 4921, 4922} {
		const n = 100000
		ecsimulate(n, ncpu, maxPosition, r)
	}
}
