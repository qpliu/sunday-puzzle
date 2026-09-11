package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) [7]int {
	correct := [7]int{}
	correct[0] = rand.Intn(4)
	for i := range 6 {
		correct[i+1] = rand.Intn(3)
		if correct[i+1] >= correct[i] {
			correct[i+1]++
		}
	}
	scores := [7]int{}
	guesses := [7]int{}
	for cheat := range 7 {
		guesses[cheat] = correct[cheat]
		for i := range 6 {
			iprev := cheat + i
			icurrent := cheat + i + 1
			if icurrent < 7 {
				guesses[icurrent] = rand.Intn(3)
				if guesses[icurrent] >= guesses[iprev] {
					guesses[icurrent]++
				}
			}
			iprev = cheat - i
			icurrent = cheat - i - 1
			if icurrent >= 0 {
				guesses[icurrent] = rand.Intn(3)
				if guesses[icurrent] >= guesses[iprev] {
					guesses[icurrent]++
				}
			}
		}
		for i, guess := range guesses {
			if guess == correct[i] {
				scores[cheat]++
			}
		}
	}
	return scores
}

func simulate(niter int, r *rand.Rand) {
	ch := make(chan [7]float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			counts := [7]int{}
			for range niter {
				for i, count := range trial(r) {
					counts[i] += count
				}
			}
			avgs := [7]float64{}
			for i, count := range counts {
				avgs[i] = float64(count) / float64(niter)
			}
			ch <- avgs
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := [7]float64{}
	for range runtime.NumCPU() {
		avgs := <-ch
		for i, avg := range avgs {
			sums[i] += avg
		}
	}
	for i, sum := range sums {
		fmt.Printf("%d:%f\n", i+1, sum/float64(runtime.NumCPU()))
	}
}

func main() {
	const seed = 20260911
	r := rand.New(rand.NewSource(seed))

	const niter = 10000000
	simulate(niter, r)
}
