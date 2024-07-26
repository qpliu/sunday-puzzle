package main

import (
	"fmt"
	"math/rand"
)

func abTrial(r *rand.Rand) bool {
	for {
		if r.Intn(6) == 0 {
			return true
		}
		if r.Intn(6) == 0 {
			return false
		}
	}
}

func abbaTrial(r *rand.Rand) bool {
	for {
		if r.Intn(6) == 0 {
			return true
		}
		if r.Intn(6) == 0 {
			return false
		}
		if r.Intn(6) == 0 {
			return false
		}
		if r.Intn(6) == 0 {
			return true
		}
	}
}

func thueMorseTrial(r *rand.Rand) bool {
	if r.Intn(6) == 0 {
		return true
	}
	history := []bool{true}
	for {
		for _, h := range(history) {
			if r.Intn(6) == 0 {
				return !h
			}
		}
		next := make([]bool, 2*len(history))
		copy(next, history)
		for i, h := range(history) {
			next[len(history)+i] = !h
		}
		history = next
	}
}

func checkThueMorse(nrounds int) {
	history := []bool{true}
	for round := range(nrounds) {
		switch round {
		case 0:
			fmt.Printf("Round 1: A\n")
		default:
			fmt.Printf("Round %d: ", round+1)
			for _, h := range(history) {
				if h {
					fmt.Printf("B")
				} else {
					fmt.Printf("A")
				}
			}
			fmt.Printf("\n")
			next := make([]bool, 2*len(history))
			copy(next, history)
			for i, h := range(history) {
				next[len(history)+i] = !h
			}
			history = next
		}
	}
}

func simulate(n int, trial func(*rand.Rand) bool, r *rand.Rand) int {
	count := 0
	for _ = range n {
		if trial(r) {
			count++
		}
	}
	return count
}

func main() {
	const n = 10000000
	const seed = 7346209
	r := rand.New(rand.NewSource(seed))
	count := simulate(n, abTrial, r)
	fmt.Printf("AB %d/%d %f\n", count, n, float64(count)/n)
	count = simulate(n, abbaTrial, r)
	fmt.Printf("ABBA %d/%d %f\n", count, n, float64(count)/n)
	count = simulate(n, thueMorseTrial, r)
	fmt.Printf("Thue-Morse %d/%d %f\n", count, n, float64(count)/n)
	checkThueMorse(6)
}
