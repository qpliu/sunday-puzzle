package main

import (
	"math"
)

func init() {
	Register(&aoc202101{
		AOC: AOC{
			Day:           1,
			InputFilename: "../2021/input/01.txt",
			Tests: []Test{
				Test{
					Input: `199
200
208
210
200
207
240
269
260
263
`,
					Part1: "7",
					Part2: "5",
				},
			},
		},
	})
}

type aoc202101 struct {
	AOC
}

func (aoc *aoc202101) Part1(input *Input) string {
	result := 0
	last := math.MaxInt
	for i := range input.IntSeq() {
		if i > last {
			result++
		}
		last = i
	}
	return IntResult(result)
}

func (aoc *aoc202101) Part2(input *Input) string {
	result := 0
	l1, l2, l3 := math.MaxInt, math.MaxInt, math.MaxInt
	for i := range input.IntSeq() {
		if i > l1 {
			result++
		}
		l1, l2, l3 = l2, l3, i
	}
	return IntResult(result)
}
