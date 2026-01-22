package main

func init() {
	Register(&aoc202001{
		AOC: AOC{
			Day:           1,
			InputFilename: "../2020/input/01.txt",
			Tests: []Test{
				Test{
					Input: `1721
979
366
299
675
1456
`,
					Part1: "514579",
					Part2: "241861950",
				},
			},
		},
	})
}

type aoc202001 struct {
	AOC
}

func (aoc *aoc202001) Part1(input *Input) string {
	seen := map[int]bool{}
	for i := range input.IntSeq() {
		if seen[2020-i] {
			return IntResult((2020 - i) * i)
		}
		seen[i] = true
	}
	panic("bad input")
}

func (aoc *aoc202001) Part2(input *Input) string {
	entries := input.Ints()
	pairs := map[int]int{}
	for i, n := range entries {
		for _, m := range entries[:i] {
			pairs[n+m] = n * m
		}
	}
	for _, n := range entries {
		if prod, ok := pairs[2020-n]; ok {
			return IntResult(prod * n)
		}
	}
	panic("bad input")
}
