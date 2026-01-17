package main

import (
	"math"
)

func init() {
	Register(&aoc202114{
		AOC: AOC{
			Day:           14,
			InputFilename: "../2021/input/14.txt",
			Tests: []Test{
				Test{
					Input: `NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C
`,
					Part1: "1588",
					Part2: "2188189693529",
				},
			},
		},
	})
}

type aoc202114 struct {
	AOC
}

func (aoc *aoc202114) parse(input *Input) (int, int, int, []int, [][4]int) {
	start, _ := input.Word()
	elements := map[byte]int{}
	pairs := map[[2]byte]int{}
	rules := [][4]int{}
	for {
		pair, _ := input.Word()
		input.Word()
		insertion, ok := input.Word()
		if !ok {
			break
		}
		left, ok := elements[pair[0]]
		if !ok {
			left = len(elements)
			elements[pair[0]] = left
		}
		right, ok := elements[pair[1]]
		if !ok {
			right = len(elements)
			elements[pair[1]] = right
		}
		mid, ok := elements[insertion[0]]
		if !ok {
			mid = len(elements)
			elements[insertion[0]] = mid
		}
		index, ok := pairs[[2]byte{pair[0], pair[1]}]
		if !ok {
			index = len(pairs)
			pairs[[2]byte{pair[0], pair[1]}] = index
			rules = append(rules, [4]int{})
		}
		leftPair, ok := pairs[[2]byte{pair[0], insertion[0]}]
		if !ok {
			leftPair = len(pairs)
			pairs[[2]byte{pair[0], insertion[0]}] = leftPair
			rules = append(rules, [4]int{})
		}
		rightPair, ok := pairs[[2]byte{insertion[0], pair[1]}]
		if !ok {
			rightPair = len(pairs)
			pairs[[2]byte{insertion[0], pair[1]}] = rightPair
			rules = append(rules, [4]int{})
		}
		rules[index][0] = leftPair
		rules[index][1] = rightPair
		rules[index][2] = left
		rules[index][3] = right
	}
	polymer := make([]int, len(pairs))
	for i := 0; i < len(start)-1; i++ {
		left := start[i]
		right := start[i+1]
		polymer[pairs[[2]byte{left, right}]]++
	}
	return elements[start[0]], elements[start[len(start)-1]], len(elements), polymer, rules
}

func (aoc *aoc202114) result(steps int, input *Input) string {
	left, right, nelements, polymer, rules := aoc.parse(input)
	tmp := make([]int, len(polymer))
	for range steps {
		clear(tmp)
		for i, n := range polymer {
			tmp[rules[i][0]] += n
			tmp[rules[i][1]] += n
		}
		polymer, tmp = tmp, polymer
	}
	elements := make([]int, nelements)
	elements[left]++
	elements[right]++
	for i, n := range polymer {
		elements[rules[i][2]] += n
		elements[rules[i][3]] += n
	}
	maxElement := 0
	minElement := math.MaxInt
	for _, n := range elements {
		maxElement = max(maxElement, n)
		minElement = min(minElement, n)
	}
	return IntResult((maxElement - minElement) / 2)
}

func (aoc *aoc202114) Part1(input *Input) string {
	return aoc.result(10, input)
}

func (aoc *aoc202114) Part2(input *Input) string {
	return aoc.result(40, input)
}
