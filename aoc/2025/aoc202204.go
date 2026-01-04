package main

func init() {
	Register(&aoc202204{
		AOC: AOC{
			Day:           4,
			InputFilename: "../2022/input/04.txt",
			Tests: []Test{
				Test{
					Input: `2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
`,
					Part1: "2",
					Part2: "4",
				},
			},
		},
	})
}

type aoc202204 struct {
	AOC
}

func (aoc *aoc202204) next(input *Input) (int, int, int, int, bool) {
	a, _ := input.Int()
	b, _ := input.Int()
	c, _ := input.Int()
	d, ok := input.Int()
	if !ok {
		return 0, 0, 0, 0, false
	}
	return a, -b, c, -d, true
}

func (aoc *aoc202204) Part1(input *Input) string {
	result := 0
	for a, b, c, d, ok := aoc.next(input); ok; a, b, c, d, ok = aoc.next(input) {
		if (a <= c && b >= d) || (c <= a && d >= b) {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202204) Part2(input *Input) string {
	result := 0
	for a, b, c, d, ok := aoc.next(input); ok; a, b, c, d, ok = aoc.next(input) {
		if !(a > d || b < c) {
			result++
		}
	}
	return IntResult(result)
}
