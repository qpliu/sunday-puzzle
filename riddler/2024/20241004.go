package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(nDice int, r *rand.Rand) (int, int) {
	rollCount := 1
	rolls := map[int]int{0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0}
	for _ = range nDice {
		rolls[r.Intn(6)]++
	}
	target := 0
	firstCount := 0
	for roll, count := range rolls {
		if count > firstCount {
			target = roll
			firstCount = count
		}
	}
	nDice -= firstCount
	for nDice > 0 {
		rollCount++
		hits := 0
		for _ = range nDice {
			if target == r.Intn(6) {
				hits++
			}
		}
		nDice -= hits
	}
	return firstCount, rollCount
}

func simulate(n, ncpu, nDice int, r *rand.Rand) {
	ch := make(chan [4]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			counts := [4]int{}
			for _ = range n {
				firstCount, rollCount := trial(nDice, r)
				counts[0] += firstCount
				counts[1] += 1
				counts[2] += nDice
				counts[3] += rollCount
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	totals := [4]int{}
	for _ = range ncpu {
		counts := <-ch
		for i := range counts {
			totals[i] += counts[i]
		}
	}
	fmt.Printf("%d: %f %f\n", nDice, float64(totals[0])/float64(totals[1]), float64(totals[2])/float64(totals[3]))
}

func main() {
	const seed = 20241004
	r := rand.New(rand.NewSource(seed))
	const n = 100000
	ncpu := runtime.NumCPU()

	simulate(n, ncpu, 3, r)
	simulate(n, ncpu, 10, r)
	simulate(n, ncpu, 25, r)
	simulate(n, ncpu, 50, r)
	simulate(n, ncpu, 100, r)
}
