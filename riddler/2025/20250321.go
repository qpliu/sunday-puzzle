package main

import (
	"fmt"
)

type team struct {
	slotSeed int
	prob     float64
	nextProb float64
}

func makeTeams(totalRounds int) []team {
	n := 1 << totalRounds
	teams := make([]team, n)
	for i := range teams {
		teams[i].slotSeed = i + 1
		teams[i].prob = 1
		teams[i].nextProb = 0
	}
	return teams
}

func doRound(remainingRounds int, teams []team) {
	slotSeedSum := 1 + 1<<remainingRounds
	for i := range teams {
		opponentSlotSeed := slotSeedSum - teams[i].slotSeed
		seed := float64(i + 1)
		for j := range teams {
			if teams[j].slotSeed != opponentSlotSeed {
				continue
			}
			opponentSeed := float64(j + 1)
			teams[i].nextProb += teams[i].prob * teams[j].prob * opponentSeed / (seed + opponentSeed)
		}
	}
	nextSlotSeedSum := 1 + 1<<(remainingRounds-1)
	for i := range teams {
		if teams[i].slotSeed >= nextSlotSeedSum {
			teams[i].slotSeed = slotSeedSum - teams[i].slotSeed
		}
		teams[i].prob = teams[i].nextProb
		teams[i].nextProb = 0
	}
}

func tourney(totalRounds int, printChecks bool) {
	teams := makeTeams(totalRounds)
	for round := range totalRounds {
		if printChecks {
			probSum := float64(0)
			for _, team := range teams {
				probSum += team.prob
			}
			fmt.Printf("Round %d (%d): %f\n", round+1, 1<<(totalRounds-round), probSum)
		}
		doRound(totalRounds-round, teams)
	}
	fmt.Printf("k=%d, p=%f\n", totalRounds, teams[0].prob)
}

func main() {
	for k := 1; k <= 15; k++ {
		tourney(k, k < 4)
	}
}
