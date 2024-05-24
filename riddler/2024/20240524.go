package main

import (
	"fmt"
	"math/rand"
)

func trial(deckSize, target int, r *rand.Rand) bool {
	deck := make([]int, deckSize)
	for i := 0; i < deckSize; i++ {
		deck[i] = i+1
	}
	rand.Shuffle(deckSize, func(i, j int) {
		deck[i], deck[j] = deck[j], deck[i]
	})
	sum := 0
	for _, c := range deck {
		sum += c
		if sum == target {
			return true
		} else if sum > target {
			return false
		}
	}
	return false
}

func simulate(deckSize, target, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if trial(deckSize, target, r) {
			wins++
		}
	}
	return wins
}

func ectrial(deckSize, target int, r *rand.Rand) bool {
	deck := make([]int, deckSize)
	for i := 0; i < deckSize; i++ {
		deck[i] = i+1
	}
	rand.Shuffle(deckSize, func(i, j int) {
		deck[i], deck[j] = deck[j], deck[i]
	})
	sum := 0
	for i, c := range deck {
		sum += c
		if sum == target {
			return true
		}
		for _, r := range deck[i+1:] {
			if sum+r > target {
				return false
			}
		}
	}
	return false
}

func ecsimulate(deckSize, target, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if ectrial(deckSize, target, r) {
			wins++
		}
	}
	return wins
}

func main() {
	const n = 10000000
	const seed = 8406112731
	const deckSize = 10
	const target = 21
	r := rand.New(rand.NewSource(seed))
	var m int
	m = simulate(deckSize, target, n, r)
	fmt.Printf("%d/%d %f\n", m, n, float64(m)/n)
	m = ecsimulate(deckSize, target, n, r)
	fmt.Printf("%d/%d %f\n", n, m, n/float64(m))
}
