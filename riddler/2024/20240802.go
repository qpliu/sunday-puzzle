package main

import (
	"fmt"
	"math/rand"
)

func trial(n int, s, t, tmp []int, r *rand.Rand) bool {
	for i := range n {
		s[i] = i
	}
	r.Shuffle(n, func(i, j int) {
		s[i], s[j] = s[j], s[i]
	})
	nCandidates := 0
	winner := 0
	for i := range n {
		if 2*i + s[i] < 2*winner + s[winner] {
			nCandidates = 0
			winner = i
		} else if 2 + s[i] < s[winner] {
			tmp[nCandidates] = i
			nCandidates++
		}
	}
	if nCandidates == 0 {
		return false
	}
	for i := range n {
		t[i] = i
	}
	r.Shuffle(n, func(i, j int) {
		t[i], t[j] = t[j], t[i]
	})
	for _, i := range tmp[:nCandidates] {
		// if s[winner]-s[i] = 3, need 1
		// if s[winner]-s[i] = 4, need 1
		// if s[winner]-s[i] = 5, need 2
		// if s[winner]-s[i] = 6, need 2
		nAfterBoth := (s[winner]-s[i]-1)/2
		count := 0
		bothFinish := max(t[i], t[winner])
		for j := winner+1; j < i; j++ {
			if t[j] > bothFinish {
				count++
				if count >= nAfterBoth {
					return true
				}
			}
		}
	}
	return false
}

func simulate(ntrials, n int, r *rand.Rand) int {
	s := make([]int, n)
	t := make([]int, n)
	tmp := make([]int, n)
	count := 0
	for _ = range ntrials {
		if trial(n, s, t, tmp, r) {
			count++
		}
	}
	return count
}

func main() {
	const ntrials = 500000
	const seed = 73462098
	r := rand.New(rand.NewSource(seed))
	for _, n := range([]int{3, 4, 5, 50, 500, 5000}) {
		count := simulate(ntrials, n, r)
		fmt.Printf("N=%d %d/%d %f\n", n, count, ntrials, float64(count)/float64(ntrials))
	}
}
