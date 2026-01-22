package main

import (
	"math/bits"
	"strings"
)

func init() {
	Register(&aoc202016{
		AOC: AOC{
			Day:           16,
			InputFilename: "../2020/input/16.txt",
			Tests: []Test{
				Test{
					Input: `class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12
`,
					Part1: "71",
					Part2: "",
				},
			},
		},
	})
}

type aoc202016 struct {
	AOC
}

func (aoc *aoc202016) parse(input *Input) ([]int, [][4]int, []int, Seq[[]int]) {
	departureFields := []int{}
	rules := [][4]int{}
	for {
		line, _ := input.Line()
		if line == "" {
			break
		}
		if strings.HasPrefix(line, "departure") {
			departureFields = append(departureFields, len(rules))
		}
		// none of the field names contain digits
		rule := InputString(line).Ints()
		rules = append(rules, [4]int{rule[0], -rule[1], rule[2], -rule[3]})
	}

	input.Line()
	line, _ := input.Line()
	myTicket := InputString(line).Ints()

	input.Line()
	input.Line()

	return departureFields, rules, myTicket, func(yield func([]int) bool) {
		for line := range input.Lines() {
			if !yield(InputString(line).Ints()) {
				return
			}
		}
	}
}

func (aoc *aoc202016) valid(rule [4]int, val int) bool {
	if val >= rule[0] && val <= rule[1] {
		return true
	}
	if val >= rule[2] && val <= rule[3] {
		return true
	}
	return false
}

func (aoc *aoc202016) invalid(rules [][4]int, val int) bool {
	for _, rule := range rules {
		if aoc.valid(rule, val) {
			return false
		}
	}
	return true
}

func (aoc *aoc202016) Part1(input *Input) string {
	_, rules, _, tickets := aoc.parse(input)
	result := 0
	for ticket := range tickets {
		for _, val := range ticket {
			if aoc.invalid(rules, val) {
				result += val
			}
		}
	}
	return IntResult(result)
}

func (aoc *aoc202016) Part2(input *Input) string {
	departureFields, rules, myTicket, tickets := aoc.parse(input)
	candidates := make([]uint, len(rules))
	{
		all := (uint(1) << len(myTicket)) - 1
		for i := range candidates {
			candidates[i] = all
		}
	}
ticketLoop:
	for ticket := range tickets {
		for _, val := range ticket {
			if aoc.invalid(rules, val) {
				continue ticketLoop
			}
		}
		for i, val := range ticket {
			bit := uint(1) << i
			for j, rule := range rules {
				if !aoc.valid(rule, val) {
					candidates[j] &^= bit
				}
			}
		}
	}
	solved := NewQueue[uint]()
	unsolved := NewQueue[int]()
	tmp := NewQueue[int]()
	for j := range candidates {
		if bits.OnesCount(candidates[j]) == 1 {
			solved.Enqueue(candidates[j])
		} else {
			unsolved.Enqueue(j)
		}
	}
	for !unsolved.Empty() {
		bit := solved.Dequeue()
		for !unsolved.Empty() {
			j := unsolved.Dequeue()
			candidates[j] &^= bit
			if bits.OnesCount(candidates[j]) == 1 {
				solved.Enqueue(candidates[j])
			} else {
				tmp.Enqueue(j)
			}
		}
		unsolved, tmp = tmp, unsolved
	}
	result := 1
	for _, j := range departureFields {
		result *= myTicket[bits.TrailingZeros(candidates[j])]
	}
	return IntResult(result)
}
