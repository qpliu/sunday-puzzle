package main

import (
	"slices"
)

func init() {
	Register(&aoc202307{
		AOC: AOC{
			Day:           7,
			InputFilename: "../2023/input/07.txt",
			Tests: []Test{
				Test{
					Input: `32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
`,
					Part1: "6440",
					Part2: "5905",
				},
			},
		},
	})
}

type aoc202307 struct {
	AOC
}

func (aoc *aoc202307) cardStrength(part1 bool, card byte) int {
	switch card {
	case '2', '3', '4', '5', '6', '7', '8', '9':
		return int(card - '0')
	case 'T':
		return 10
	case 'J':
		if part1 {
			return 11
		} else {
			return 0
		}
	case 'Q':
		return 12
	case 'K':
		return 13
	case 'A':
		return 14
	default:
		panic("bad input")
	}
}

func (aoc *aoc202307) handStrength(part1 bool, hand string) int {
	handType := 0
	counts := map[int]int{}
	for _, card := range []byte(hand) {
		counts[aoc.cardStrength(part1, card)]++
	}
	matches := [6]int{counts[0]}
	i := 1
	for strength, count := range counts {
		if strength != 0 && count > 0 {
			matches[i] = count
			i++
		}
	}
	slices.SortFunc(matches[1:], func(a, b int) int {
		return b - a
	})
	switch matches {
	case [6]int{0, 5}, [6]int{1, 4}, [6]int{2, 3}, [6]int{3, 2}, [6]int{4, 1}, [6]int{5}:
		handType = 6
	case [6]int{0, 4, 1}, [6]int{1, 3, 1}, [6]int{2, 2, 1}, [6]int{3, 1, 1}:
		handType = 5
	case [6]int{0, 3, 2}, [6]int{1, 2, 2}:
		handType = 4
	case [6]int{0, 3, 1, 1}, [6]int{1, 2, 1, 1}, [6]int{2, 1, 1, 1}:
		handType = 3
	case [6]int{0, 2, 2, 1}:
		handType = 2
	case [6]int{0, 2, 1, 1, 1}, [6]int{1, 1, 1, 1, 1}:
		handType = 1

	}
	return (handType << (5 * 4)) |
		(aoc.cardStrength(part1, hand[0]) << (4 * 4)) |
		(aoc.cardStrength(part1, hand[1]) << (3 * 4)) |
		(aoc.cardStrength(part1, hand[2]) << (2 * 4)) |
		(aoc.cardStrength(part1, hand[3]) << (1 * 4)) |
		(aoc.cardStrength(part1, hand[4]) << (0 * 4))
}

func (aoc *aoc202307) parse(part1 bool, input *Input) [][2]int {
	hands := [][2]int{}
	for {
		hand, ok := input.Word()
		if !ok {
			break
		}
		bid, ok := input.Int()
		if !ok {
			break
		}
		hands = append(hands, [2]int{aoc.handStrength(part1, hand), bid})
	}
	return hands
}

func (aoc *aoc202307) result(part1 bool, input *Input) string {
	hands := aoc.parse(part1, input)
	slices.SortFunc(hands, func(a, b [2]int) int {
		return a[0] - b[0]
	})

	result := 0
	for i, hand := range hands {
		result += (i + 1) * hand[1]
	}
	return IntResult(result)
}

func (aoc *aoc202307) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202307) Part2(input *Input) string {
	return aoc.result(false, input)
}
