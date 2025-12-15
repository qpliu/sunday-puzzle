package main

func init() {
	Register(&aoc202503{
		AOC: AOC{
			Day: 3,
			Tests: []Test{
				Test{
					Input: `987654321111111
811111111111119
234234234234278
818181911112111
`,
					Part1: "357",
					Part2: "3121910778619",
				},
			},
		},
	})
}

type aoc202503 struct {
	AOC
}

func (aoc *aoc202503) joltage(bank string, count int) int {
	result := 0
	i := 0
	for count > 0 {
		count--
		result *= 10
		imax := i
		for j := range len(bank) - i - count {
			if bank[i+j] > bank[imax] {
				imax = i + j
			}
		}
		result += int(bank[imax] - '0')
		i = imax + 1
	}
	return result
}

func (aoc *aoc202503) Part1(input *Input) string {
	result := 0
	for bank, ok := input.Line(); ok; bank, ok = input.Line() {
		result += aoc.joltage(bank, 2)
	}
	return IntResult(result)
}

func (aoc *aoc202503) Part2(input *Input) string {
	result := 0
	for bank, ok := input.Line(); ok; bank, ok = input.Line() {
		result += aoc.joltage(bank, 12)
	}
	return IntResult(result)
}
