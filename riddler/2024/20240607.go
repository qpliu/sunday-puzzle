package main

import (
	"fmt"
	"math"
	"math/rand"
)

func trial(npossessions int, thresholdTable map[[2]int]bool, r *rand.Rand) (bool, bool) {
	scores := [2]int{0, 0}
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

func choose(n, k int) float64 {
	result := float64(1)
	if n-k > k {
		k = n - k
	}
	for i := k + 1; i <= n; i++ {
		result *= float64(i)
	}
	for i := 2; i <= n-k; i++ {
		result /= float64(i)
	}
	return result
}

func makeThresholdTable(npossessions int, threshold float64) map[[2]int]bool {
	thresholdTable := make(map[[2]int]bool)
	for us := 0; us <= npossessions-1; us++ {
		for them := 0; them <= npossessions-1-us; them++ {
			n := npossessions - us - them
			counts := float64(0)
			for i := npossessions/2 + 1 - them; i <= n; i++ {
				counts += choose(n, i)
			}
			if counts/math.Pow(2, float64(n)) >= threshold {
				thresholdTable[[2]int{us, them}] = true
			}
		}
	}
	return thresholdTable
}

func simulate(n, npossessions int, threshold float64, r *rand.Rand) (int, int) {
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
	const n = 5000000
	const seed = 740683
	r := rand.New(rand.NewSource(seed))
	var npossessions int
	var threshold float64
	run := func() {
		ng, nw := simulate(n, npossessions, threshold, r)
		fmt.Printf("%d %f %d/%d %f %f\n", npossessions, threshold, nw, ng, float64(nw)/float64(ng), float64(ng)/n)
	}
	npossessions = 5
	threshold = 0.75
	run()
	npossessions = 101
	threshold = 0.9
	run()
}
