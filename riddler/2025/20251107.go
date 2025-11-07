package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(nPresses int, pr float64, r *rand.Rand) bool {
	atDoor2 := true
	for _ = range nPresses {
		if atDoor2 {
			atDoor2 = r.Float64() < 0.2
		} else {
			atDoor2 = r.Float64() > pr
		}
	}
	return atDoor2
}

func simulate(niters, ncpu, nPresses int, pr float64, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range niters {
				if trial(nPresses, pr, r) {
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
	fmt.Printf("f:pr:%f %f\n", pr, float64(count)/float64(niters*ncpu))
}

func ectrial(atDoor2 bool, nPresses int, pr float64, r *rand.Rand) bool {
	for _ = range nPresses {
		if atDoor2 {
			atDoor2 = r.Float64() < 0.2
		} else {
			atDoor2 = r.Float64() > pr
		}
		if atDoor2 {
			atDoor2 = r.Float64() < 0.5
		} else {
			atDoor2 = r.Float64() > pr
		}
	}
	return atDoor2
}

func ecsimulate(startAtDoor2 bool, niters, ncpu, nPresses int, pr float64, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range niters {
				if ectrial(startAtDoor2, nPresses, pr, r) {
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
	fmt.Printf("ec:pr:%f %f\n", pr, float64(count)/float64(niters*ncpu))
}

func main() {
	const seed = 20251107
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 100000
	const nPresses = 1000
	simulate(niters, ncpu, nPresses, 0.5, r)
	simulate(niters, ncpu, nPresses, 0.6, r)
	simulate(niters, ncpu, nPresses, 0.7, r)

	ecsimulate(true, niters, ncpu, nPresses, math.Sqrt(181)/20, r)
	ecsimulate(true, niters, ncpu, nPresses, (math.Sqrt(181)+1)/20, r)
	ecsimulate(true, niters, ncpu, nPresses, (math.Sqrt(181)+2)/20, r)

	ecsimulate(false, niters, ncpu, nPresses, math.Sqrt(181)/20, r)
	ecsimulate(false, niters, ncpu, nPresses, (math.Sqrt(181)+1)/20, r)
	ecsimulate(false, niters, ncpu, nPresses, (math.Sqrt(181)+2)/20, r)
}
