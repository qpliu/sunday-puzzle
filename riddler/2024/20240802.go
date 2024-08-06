package main

import (
	"fmt"
	"math/rand"
	"sort"
)

func trial(n int, bruteForceOrder bool, s, t, tmp []int, r *rand.Rand) (bool, bool) {
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
	for i := range n {
		t[i] = i
	}
	r.Shuffle(n, func(i, j int) {
		t[i], t[j] = t[j], t[i]
	})

	gap := false
	for _, i := range tmp[:nCandidates] {
		countBetween := 0
		bothFinish := max(t[i], t[winner])
		for j := winner+1; j < i; j++ {
			if t[j] < bothFinish {
				countBetween++
			}
		}
		if s[winner] > 2 + 2*countBetween + s[i] {
			gap = true
			break
		}
	}

	bruteForce := false
	if bruteForceOrder {
		copy(tmp, t)
		for i, j := range tmp {
			t[j] = i
		}
		for i := range n {
			sort.Ints(t[:i+1])
			haveWinner := winner == t[0]
			leader := t[0]
			leaderScore := s[leader]
			for j := range i+1 {
				if t[j] == winner {
					haveWinner = true
				}
				if 2*j + s[t[j]] < leaderScore {
					leader = t[j]
					leaderScore = 2*j + s[t[j]]
				}
			}
			if haveWinner && leader != winner {
				bruteForce = true
				break
			}
		}
	}

	return gap, bruteForce
}

func simulate(ntrials, n int, bruteForceOrder bool, r *rand.Rand) (int, int) {
	s := make([]int, n)
	t := make([]int, n)
	tmp := make([]int, n)
	countGap := 0
	countBruteForce := 0
	for _ = range ntrials {
		gap, bruteForce := trial(n, bruteForceOrder, s, t, tmp, r)
		if gap {
			countGap++
		}
		if bruteForce {
			countBruteForce++
		}
	}
	return countGap, countBruteForce
}

func main() {
	const ntrials = 100000
	const seed = 73462091
	r := rand.New(rand.NewSource(seed))
	for _, n := range([]int{3, 4, 5, 6, 8, 10, 50, 500, 5000, 10000}) {
		bruteForceOrder := false
		countGap, countBruteForce := simulate(ntrials, n, bruteForceOrder, r)
		if bruteForceOrder {
			fmt.Printf("brute force N=%d %d/%d %f\n", n, countBruteForce, ntrials, float64(countBruteForce)/float64(ntrials))
		}
		fmt.Printf("N=%d %d/%d %f\n", n, countGap, ntrials, float64(countGap)/float64(ntrials))
	}
}
