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

func fttrial(ranks, suits int, r *rand.Rand) bool {
	deckSize := ranks*suits
	deck1 := make([]int, deckSize)
	deck2 := make([]int, deckSize)
	for i := 0; i < deckSize; i++ {
		deck1[i] = i%ranks
		deck2[i] = i%ranks
	}
	rand.Shuffle(deckSize, func(i, j int) {
		deck1[i], deck1[j] = deck1[j], deck1[i]
	})
	rand.Shuffle(deckSize, func(i, j int) {
		deck2[i], deck2[j] = deck2[j], deck2[i]
	})
	for i := 0; i < deckSize; i++ {
		if deck1[i] == deck2[i] {
			return false
		}
	}
	return true
}

func ftsimulate(ranks, suits, n int, r *rand.Rand) int {
	wins := 0
	for i := 0; i < n; i++ {
		if fttrial(ranks, suits, r) {
			wins++
		}
	}
	return wins
}

func main() {
	const n = 1000000
	const seed = 8406112731
	const deckSize = 52
	r := rand.New(rand.NewSource(seed))
	var m int
	m = simulate(deckSize, n, r)
	fmt.Printf("%d/%d %f\n", m, n, float64(m)/n)
	m = ecsimulate(deckSize, n, r)
	fmt.Printf("%d/%d %f\n", m, n, float64(m)/n)
	f := func(ranks, suits int) {
		p := ftsimulate(ranks, suits, n, r)
		fmt.Printf("ranks=%d suits=%d %d/%d %f\n", ranks, suits, p, n, float64(p)/n)
	}
	f(13, 4)
	f(2, 2)
	f(3, 2)
	f(4, 2)
	f(5, 2)
	f(2, 3)
	f(3, 3)
	f(4, 3)
	f(5, 3)
	f(2, 4)
	f(3, 4)
	f(4, 4)
	f(5, 4)
}
