package main

import (
	"fmt"
	"math/rand"
)

func trial(r *rand.Rand, board []int, ndice int) {
	p := 0
	for p < len(board) {
		board[p]++
		for i := 0; i < ndice; i++ {
			p += r.Intn(6) + 1
		}
	}
}

func simulate(ntrials int, r *rand.Rand, ndice int) [40]int {
	board := [40]int{}
	for i := 0; i < ntrials; i++ {
		trial(r, board[:], ndice)
	}
	return board
}

func main() {
	const ntrials = 10000000
	const seed = 3851367
	r := rand.New(rand.NewSource(seed))
	ndice := 2
	for i, n := range simulate(ntrials, r, ndice) {
		fmt.Printf("%d: %d %f\n", i, n, float64(n)/ntrials)
	}
	ndice = 3
	for i, n := range simulate(ntrials, r, ndice) {
		fmt.Printf("%d: %d %f\n", i, n, float64(n)/ntrials)
	}
}
