package main

import (
	"fmt"
	"math/rand"
	"os"
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

func pdistTrial(nDice int, dist [][6]int, r *rand.Rand) {
	rolls := map[int]int{0: 0, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0}
	for _ = range nDice {
		rolls[r.Intn(6)]++
	}
	firstCount := 0
	numCount := 0
	for _, count := range rolls {
		if count > firstCount {
			firstCount = count
			numCount = 1
		} else if count == firstCount {
			numCount++
		}
	}
	dist[firstCount-1][numCount-1]++
}

func pdist(n, ncpu, nDice int, r *rand.Rand) {
	ch := make(chan [][6]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			dist := make([][6]int, nDice)
			for _ = range n {
				pdistTrial(nDice, dist, r)
			}
			ch <- dist
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	totals := make([][6]int, nDice)
	for _ = range ncpu {
		dist := <-ch
		for i := range dist {
			for j := range dist[i] {
				totals[i][j] += dist[i][j]
			}
		}
	}
	for i, tots := range totals {
		tot := 0
		isSplit := false
		for k, count := range tots {
			if count > 0 {
				fmt.Printf("%d %d,%d: %g\n", nDice, i+1, k+1, float64(count)/float64(n*ncpu))
				if tot > 0 {
					isSplit = true
				}
				tot += count
			}
		}
		if isSplit {
			fmt.Printf("%d %d: %g\n", nDice, i+1, float64(tot)/float64(n*ncpu))
		}
	}
}

func pdistBruteForceTrial(nDice, preset, roll int, dist [][6]int) {
	// make the first die always 0 to cut down the calculations
	// by a factor of 6
	// make the second die always set to preset for parallelization
	rolls := map[int]int{0: 1, 1: 0, 2: 0, 3: 0, 4: 0, 5: 0}
	rolls[preset]++
	for _ = range nDice - 2 {
		rolls[roll%6]++
		roll /= 6
	}
	firstCount := 0
	numCount := 0
	for _, count := range rolls {
		if count > firstCount {
			firstCount = count
			numCount = 1
		} else if count == firstCount {
			numCount++
		}
	}
	dist[firstCount-1][numCount-1]++
}

func pdistBruteForce(nDice int) {
	n := 1
	for _ = range nDice - 2 {
		n *= 6
	}
	ch := make(chan [][6]int)
	for i := range 6 {
		go func(preset int) {
			dist := make([][6]int, nDice)
			for roll := range n {
				pdistBruteForceTrial(nDice, preset, roll, dist)
			}
			ch <- dist
		}(i)
	}
	totals := make([][6]int, nDice)
	for _ = range 6 {
		dist := <-ch
		for i := range dist {
			for j := range dist[i] {
				totals[i][j] += dist[i][j]
			}
		}
	}
	for i, tots := range totals {
		tot := 0
		isSplit := false
		for k, count := range tots {
			if count > 0 {
				fmt.Printf("%d %d,%d: %d/%d %g\n", nDice, i+1, k+1, count, n*6, float64(count)/float64(n*6))
				if tot > 0 {
					isSplit = true
				}
				tot += count
			}
		}
		if isSplit {
			fmt.Printf("%d %d: %d/%d %g\n", nDice, i+1, tot, n*6, float64(tot)/float64(n*6))
		}
	}
}

func main() {
	const seed = 20241004
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	if len(os.Args) == 2 && os.Args[1] == "pdist" {
		pdistBruteForce(3)
		pdistBruteForce(4)

		const n = 5000000
		pdist(n, ncpu, 10, r)
	} else {
		const n = 100000
		simulate(n, ncpu, 3, r)
		simulate(n, ncpu, 6, r)
		simulate(n, ncpu, 10, r)
		simulate(n, ncpu, 12, r)
		simulate(n, ncpu, 25, r)
		simulate(n, ncpu, 50, r)
		simulate(n, ncpu, 60, r)
		simulate(n, ncpu, 100, r)
	}
}
