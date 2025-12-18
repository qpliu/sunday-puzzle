package main

import (
	"slices"
)

func init() {
	Register(&aoc202401{
		AOC: AOC{
			Day:           1,
			InputFilename: "../2024/input/01.txt",
			Tests: []Test{
				Test{
					Input: `3   4
4   3
2   5
1   3
3   9
3   3
`,
					Part1: "11",
					Part2: "31",
				},
			},
		},
	})
}

type aoc202401 struct {
	AOC
}

func (aoc *aoc202401) Part1(input *Input) string {
	var left, right []int
	for {
		l, _ := input.Int()
		r, ok := input.Int()
		if !ok {
			break
		}
		left = append(left, l)
		right = append(right, r)
	}
	slices.Sort(left)
	slices.Sort(right)

	dist := 0
	for i, l := range left {
		r := right[i]
		dist += max(l-r, r-l)
	}
	return IntResult(dist)
}

func (aoc aoc202401) Part2(input *Input) string {
	var left []int
	right := map[int]int{}
	for {
		l, _ := input.Int()
		r, ok := input.Int()
		if !ok {
			break
		}
		left = append(left, l)
		right[r]++
	}

	score := 0
	for _, l := range left {
		score += l * right[l]
	}
	return IntResult(score)
}
