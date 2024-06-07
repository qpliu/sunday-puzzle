package main

import (
	"fmt"
	"math/rand"
)

func trial(npossessions int, thresholdTable map[[2]int]bool, r *rand.Rand) (bool,bool) {
	scores := [2]int{0,0}
	metThreshold := false
	for i := 0; i < npossessions; i++ {
		if r.Intn(2) == 0 {
			scores[0]++
		} else {
			scores[1]++
		}
		if !metThreshold && thresholdTable[scores] {
			metThreshold = true
		}
	}
	return metThreshold, scores[0] > scores[1]
}

func makeThresholdTable(npossessions int, threshold float64) map[[2]int]bool {
	panic("not implemented")
}

func simulate(n, npossessions int, threshold float64, r *rand.Rand) (int,int) {
	nGames := 0
	nWins := 0
	thresholdTable := makeThresholdTable(npossessions, threshold)
	for i := 0; i < n; i++ {
		metThreshold, won := trial(npossessions, thresholdTable, r)
		if metThreshold {
			nGames++
			if won {
				nWins++
			}
		}
	}
	return nGames, nWins
}

func main() {
	const n = 1000000
	const seed = 74068
	r := rand.New(rand.NewSource(seed))
	var npossessions int
	var threshold float64
	run := func() {
		ng, nw := simulate(n, npossessions, threshold, r)
		fmt.Printf("%d %f %d/%d %f\n", npossessions, threshold, nw, ng, float64(nw)/float64(ng))
	}
	npossessions = 5
	threshold = 0.75
	run()
	npossessions = 101
	threshold = 0.9
	run()
}
