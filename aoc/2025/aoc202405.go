package main

import (
	"slices"
)

func init() {
	Register(&aoc202405{
		AOC: AOC{
			Day:           5,
			InputFilename: "../2024/input/05.txt",
			Tests: []Test{
				Test{
					Input: `47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
`,
					Part1: "143",
					Part2: "123",
				},
			},
		},
	})
}

type aoc202405 struct {
	AOC
}

func (aoc *aoc202405) rules(input *Input) map[[2]int]bool {
	rules := map[[2]int]bool{}
	for {
		line, _ := input.Line()
		if line == "" {
			return rules
		}
		in := InputString(line)
		x, _ := in.Int()
		y, _ := in.Int()
		rules[[2]int{x, y}] = true
	}
}

func (aoc *aoc202405) pages(input *Input) ([]int, bool) {
	line, ok := input.Line()
	if !ok {
		return nil, false
	}
	return InputString(line).Ints(), true
}

func (aoc *aoc202405) ordered(rules map[[2]int]bool, pages []int) bool {
	for i, p := range pages[1:] {
		if !rules[[2]int{pages[i], p}] {
			return false
		}
	}
	return true
}

func (aoc *aoc202405) Part1(input *Input) string {
	rules := aoc.rules(input)

	result := 0
	for pages, ok := aoc.pages(input); ok; pages, ok = aoc.pages(input) {
		if aoc.ordered(rules, pages) {
			result += pages[len(pages)/2]
		}
	}
	return IntResult(result)
}

func (aoc aoc202405) Part2(input *Input) string {
	rules := aoc.rules(input)

	result := 0
	for pages, ok := aoc.pages(input); ok; pages, ok = aoc.pages(input) {
		if !aoc.ordered(rules, pages) {
			slices.SortFunc(pages, func(a, b int) int {
				if a == b {
					return 0
				} else if rules[[2]int{a, b}] {
					return -1
				} else {
					return 1
				}
			})
			// doesn't matter whether it is sorted ascending or descending
			result += pages[len(pages)/2]
		}
	}
	return IntResult(result)
}
