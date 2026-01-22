package main

func init() {
	Register(&aoc202015{
		AOC: AOC{
			Day:           15,
			InputFilename: "../2020/input/15.txt",
			Tests: []Test{
				Test{
					Input: `0,3,6
`,
					Part1: "436",
					Part2: "", //"175594",
				},
				Test{
					Input: `1,3,2
`,
					Part1: "1",
					Part2: "", //"2578",
				},
				Test{
					Input: `2,1,3
`,
					Part1: "10",
					Part2: "", //"3544142",
				},
				Test{
					Input: `1,2,3
`,
					Part1: "27",
					Part2: "", //"261214",
				},
				Test{
					Input: `2,3,1
`,
					Part1: "78",
					Part2: "", //"6895259",
				},
				Test{
					Input: `3,2,1
`,
					Part1: "438",
					Part2: "", //"18",
				},
				Test{
					Input: `3,1,2
`,
					Part1: "1836",
					Part2: "", //"362",
				},
			},
		},
	})
}

type aoc202015 struct {
	AOC
}

func (aoc *aoc202015) result(limit int, input *Input) string {
	turn := 0
	history := make([]int, limit)
	last := 0
	for n := range input.IntSeq() {
		if turn > 0 {
			history[last] = turn
		}
		turn++
		last = n
	}
	for turn < limit {
		n := 0
		if history[last] > 0 {
			n = turn - history[last]
		}
		history[last] = turn
		turn++
		last = n
	}
	return IntResult(last)
}

func (aoc *aoc202015) Part1(input *Input) string {
	return aoc.result(2020, input)
}

func (aoc *aoc202015) Part2(input *Input) string {
	return aoc.result(30000000, input)
}
