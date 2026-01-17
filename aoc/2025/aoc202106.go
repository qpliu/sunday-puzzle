package main

func init() {
	Register(&aoc202106{
		AOC: AOC{
			Day:           6,
			InputFilename: "../2021/input/06.txt",
			Tests: []Test{
				Test{
					Input: `3,4,3,1,2
`,
					Part1: "5934",
					Part2: "26984457539",
				},
			},
		},
	})
}

type aoc202106 struct {
	AOC
}

func (aoc *aoc202106) result(days int, input *Input) string {
	counts := [9]int{}

	for i := range input.IntSeq() {
		counts[i]++
	}

	for range days {
		c0 := counts[0]
		copy(counts[:], counts[1:])
		counts[6] += c0
		counts[8] = c0
	}

	result := 0
	for _, count := range counts {
		result += count
	}
	return IntResult(result)
}

func (aoc *aoc202106) Part1(input *Input) string {
	return aoc.result(80, input)
}

func (aoc *aoc202106) Part2(input *Input) string {
	return aoc.result(256, input)
}
