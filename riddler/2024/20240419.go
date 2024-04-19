package main

import (
	"fmt"
	"math/rand"
)

func trial(debug bool, deckSize int, r *rand.Rand) bool {
	deck1 := make([]int, deckSize)
	deck2 := make([]int, deckSize)
	for i := 0; i < deckSize; i++ {
		deck1[i] = i
		deck2[i] = i
	}
	rand.Shuffle(deckSize, func(i, j int) {
		deck1[i], deck1[j] = deck1[j], deck1[i]
	})
	rand.Shuffle(deckSize, func(i, j int) {
		deck2[i], deck2[j] = deck2[j], deck2[i]
	})
	for i := 0; i < deckSize; i++ {
		if deck1[i] == deck2[i] {
			if debug {
				fmt.Println("deck1:%d deck2:%d lose", deck1, deck2)
			}
			return false
		}
	}
	if debug {
		fmt.Println("deck1:%d deck2:%d win", deck1, deck2)
	}
	return true
}

func simulate(deckSize, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if trial(i < 10 && deckSize < 5, deckSize, r) {
			wins++
		}
	}
	return wins
}

func ectrial(deckSize int, r *rand.Rand) bool {
	deck := make([]int, 2*deckSize)
	for i := 0; i < deckSize; i++ {
		deck[i] = i
		deck[i+deckSize] = i
	}
	rand.Shuffle(2*deckSize, func(i, j int) {
		deck[i], deck[j] = deck[j], deck[i]
	})
	for i := 0; i < deckSize; i++ {
		if deck[i] == deck[i+deckSize] {
			return false
		}
	}
	return true
}

func ecsimulate(deckSize, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if ectrial(deckSize, r) {
			wins++
		}
	}
	return wins
}

func main() {
	const n = 10000000
	const seed = 84061273
	const deckSize = 52
	r := rand.New(rand.NewSource(seed))
	m := simulate(deckSize, n, r)
	fmt.Printf("%d/%d %f\n", m, n, float64(m)/n)
	m = ecsimulate(deckSize, n, r)
	fmt.Printf("%d/%d %f\n", m, n, float64(m)/n)
}
