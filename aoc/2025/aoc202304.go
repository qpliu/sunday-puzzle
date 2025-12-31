package main

func init() {
	Register(&aoc202304{
		AOC: AOC{
			Day:           4,
			InputFilename: "../2023/input/04.txt",
			Tests: []Test{
				Test{
					Input: `Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
`,
					Part1: "13",
					Part2: "30",
				},
			},
		},
	})
}

type aoc202304 struct {
	AOC
}

func (aoc *aoc202304) next(input *Input) (int, bool) {
	line, ok := input.Line()
	if !ok {
		return 0, false
	}
	input = InputString(line)
	input.Word()
	input.Word()

	winners := map[string]bool{}
	for num, ok := input.Word(); ok; num, ok = input.Word() {
		if num == "|" {
			break
		}
		winners[num] = true
	}
	wins := 0
	for num, ok := input.Word(); ok; num, ok = input.Word() {
		if winners[num] {
			wins++
		}
	}
	return wins, true
}

func (aoc *aoc202304) Part1(input *Input) string {
	result := 0
	for wins, ok := aoc.next(input); ok; wins, ok = aoc.next(input) {
		if wins > 0 {
			result += 1 << (wins - 1)
		}
	}
	return IntResult(result)
}

func (aoc *aoc202304) Part2(input *Input) string {
	result := 0
	copies := map[int]int{}
	card := 1
	for wins, ok := aoc.next(input); ok; wins, ok = aoc.next(input) {
		count := copies[card] + 1
		result += count
		card++
		for i := range wins {
			copies[card+i] += count
		}
	}
	return IntResult(result)
}
