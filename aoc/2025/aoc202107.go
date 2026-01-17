package main

import (
	"math"
	"slices"
)

func init() {
	Register(&aoc202107{
		AOC: AOC{
			Day:           7,
			InputFilename: "../2021/input/07.txt",
			Tests: []Test{
				Test{
					Input: `16,1,2,0,4,2,7,1,2,14
`,
					Part1: "37",
					Part2: "168",
				},
			},
		},
	})
}

type aoc202107 struct {
	AOC
}

func (aoc *aoc202107) Part1(input *Input) string {
	positions := input.Ints()
	slices.Sort(positions)
	median := (positions[len(positions)/2-1] + positions[len(positions)/2]) / 2
	cost := 0
	for _, pos := range positions {
		cost += max(pos-median, median-pos)
	}
	return IntResult(cost)
}

func (aoc *aoc202107) Part2(input *Input) string {
	positions := input.Ints()
	sum := 0
	for _, pos := range positions {
		sum += pos
	}
	mean := sum / len(positions)
	minCost := math.MaxInt
	for _, offset := range [...]int{-1, 0, 1} {
		cost := 0
		for _, pos := range positions {
			dx := max(pos-mean-offset, mean+offset-pos)
			cost += dx * (dx + 1) / 2
		}
		minCost = min(cost, minCost)
	}
	return IntResult(minCost)
}
