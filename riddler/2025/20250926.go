package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func canExchange(card1, card2, card3 int) bool {
	switch {
	case card1 == 0 || card2 == 0 || card3 == 0:
		return true
	case card1 == card2 && card2 == card3:
		return true
	case card1 != card2 && card2 != card3 && card3 != card1:
		return true
	default:
		return false
	}
}

func trial(deck []int, r *rand.Rand) (bool, int) {
	r.Shuffle(len(deck), func(i, j int) {
		deck[i], deck[j] = deck[j], deck[i]
	})
	first3 := canExchange(deck[0], deck[1], deck[2])

	switch {
	case canExchange(deck[0], deck[1], deck[2]):
		return first3, 3
	case canExchange(deck[0], deck[1], deck[3]):
		return first3, 4
	case canExchange(deck[0], deck[2], deck[3]):
		return first3, 4
	case canExchange(deck[1], deck[2], deck[3]):
		return first3, 4
	default:
		return first3, 5
	}
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			deck := make([]int, 42)
			for i := range 14 {
				deck[3*i] = 1
				deck[3*i+1] = 2
				deck[3*i+2] = 3
			}
			count := 0
			for _ = range niters {
				if first3, _ := trial(deck, r); first3 {
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
	fmt.Printf("%f\n", float64(count)/float64(niters*ncpu))
}

func ecsimulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			deck := make([]int, 44)
			for i := range 14 {
				deck[3*i] = 1
				deck[3*i+1] = 2
				deck[3*i+2] = 3
			}
			count := 0
			for _ = range niters {
				_, n := trial(deck, r)
				count += n
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}
	fmt.Printf("%f\n", float64(count)/float64(niters*ncpu))
}

func main() {
	const seed = 20250926
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 2000000
	simulate(niters, ncpu, r)
	ecsimulate(niters, ncpu, r)
}
