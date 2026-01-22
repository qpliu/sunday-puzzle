package main

import (
	"math"
)

func init() {
	Register(&aoc202005{
		AOC: AOC{
			Day:           5,
			InputFilename: "../2020/input/05.txt",
			Tests: []Test{
				Test{
					Input: `
`,
					Part1: "",
					Part2: "",
				},
			},
		},
	})
}

type aoc202005 struct {
	AOC
}

func (aoc *aoc202005) parse(input *Input) Seq[int] {
	return func(yield func(int) bool) {
		for w := range input.Words() {
			id := 0
			bit := 512
			for _, ch := range w {
				switch ch {
				case 'B', 'R':
					id |= bit
				}
				bit >>= 1
			}
			if !yield(id) {
				return
			}
		}
	}
}

func (aoc *aoc202005) Part1(input *Input) string {
	result := 0
	for id := range aoc.parse(input) {
		result = max(result, id)
	}
	return IntResult(result)
}

func (aoc *aoc202005) Part2(input *Input) string {
	first := math.MaxInt
	last := 0
	set := map[int]bool{}
	for id := range aoc.parse(input) {
		set[id] = true
		first = min(id, first)
		last = max(id, last)
	}
	for i := first + 1; i < last; i++ {
		if !set[i] {
			return IntResult(i)
		}
	}
	panic("bad input")
}
