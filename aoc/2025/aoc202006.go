package main

import (
	"math/bits"
)

func init() {
	Register(&aoc202006{
		AOC: AOC{
			Day:           6,
			InputFilename: "../2020/input/06.txt",
			Tests: []Test{
				Test{
					Input: `abc

a
b
c

ab
ac

a
a
a
a

b
`,
					Part1: "11",
					Part2: "6",
				},
			},
		},
	})
}

type aoc202006 struct {
	AOC
}

func (aoc *aoc202006) parse(input *Input) Seq[[]int] {
	return func(yield func([]int) bool) {
		for paragraph, ok := input.Paragraph(); ok; paragraph, ok = input.Paragraph() {
			group := []int{}
			for word := range InputString(paragraph).Words() {
				p := 0
				for _, ch := range word {
					p |= 1 << int(ch-'a')
				}
				group = append(group, p)
			}
			if !yield(group) {
				return
			}
		}
	}
}

func (aoc *aoc202006) Part1(input *Input) string {
	result := 0
	for group := range aoc.parse(input) {
		res := 0
		for _, ans := range group {
			res |= ans
		}
		result += bits.OnesCount(uint(res))
	}
	return IntResult(result)
}

func (aoc *aoc202006) Part2(input *Input) string {
	result := 0
	for group := range aoc.parse(input) {
		res := -1
		for _, ans := range group {
			res &= ans
		}
		result += bits.OnesCount(uint(res))
	}
	return IntResult(result)
}
