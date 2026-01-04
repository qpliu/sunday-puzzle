package main

func init() {
	Register(&aoc202202{
		AOC: AOC{
			Day:           2,
			InputFilename: "../2022/input/02.txt",
			Tests: []Test{
				Test{
					Input: `A Y
B X
C Z
`,
					Part1: "15",
					Part2: "12",
				},
			},
		},
	})
}

type aoc202202 struct {
	AOC
}

func (aoc *aoc202202) games(input *Input) Seq2[int, int] {
	return func(yield func(int, int) bool) {
		for {
			opp, _ := input.Word()
			you, ok := input.Word()
			if !ok {
				return
			}
			if !yield(int(opp[0]-'A'), int(you[0]-'X')) {
				return
			}
		}
	}
}

func (aoc *aoc202202) Part1(input *Input) string {
	result := 0
	for opp, you := range aoc.games(input) {
		switch (opp + 3 - you) % 3 {
		case 0:
			result += 4 + you
		case 1:
			result += 1 + you
		case 2:
			result += 7 + you
		}
	}
	return IntResult(result)
}

func (aoc *aoc202202) Part2(input *Input) string {
	result := 0
	for opp, you := range aoc.games(input) {
		switch you {
		case 0:
			result += 1 + (opp+2)%3
		case 1:
			result += 4 + opp
		case 2:
			result += 7 + (opp+1)%3
		}
	}
	return IntResult(result)
}
