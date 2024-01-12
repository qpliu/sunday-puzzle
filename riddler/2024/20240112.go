package main

import (
	"fmt"
	"math/rand"
)

var dice = []int{4,6,8,10,12,20}

func trial(r *rand.Rand) bool {
	s1 := dice[r.Intn(len(dice))]
	s2 := dice[r.Intn(len(dice))]
	return r.Intn(s1) == r.Intn(s2)
}

func simulate(ntrials int, r *rand.Rand) int {
	nsame := 0
	for i := 0; i < ntrials; i++ {
		if trial(r) {
			nsame++
		}
	}
	return nsame
}

func ectrial(r *rand.Rand) int {
	r1 := r.Intn(dice[r.Intn(len(dice))])
	r2 := r.Intn(dice[r.Intn(len(dice))])
	r3 := r.Intn(dice[r.Intn(len(dice))])
	if r1 == r2 {
		if r2 == r3 {
			return 1
		} else {
			return 2
		}
	} else if r1 == r3 || r2 == r3 {
		return 2
	} else {
		return 3
	}
}

func ecsimulate(ntrials int, r *rand.Rand) int {
	ndistinct := 0
	for i := 0; i < ntrials; i++ {
		ndistinct += ectrial(r)
	}
	return ndistinct
}

func main() {
	const ntrials = 10000000
	const seed = 38513678
	r := rand.New(rand.NewSource(seed))
	n := simulate(ntrials, r)
	fmt.Printf("%d/%d %f\n", n, ntrials, float64(n)/ntrials)
	n = ecsimulate(ntrials, r)
	fmt.Printf("%d/%d %f\n", n, ntrials, float64(n)/ntrials)
}
