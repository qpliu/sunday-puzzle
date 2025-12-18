package main

func init() {
	Register(&aoc202402{
		AOC: AOC{
			Day:           2,
			InputFilename: "../2024/input/02.txt",
			Tests: []Test{
				Test{
					Input: `7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
`,
					Part1: "2",
					Part2: "4",
				},
			},
		},
	})
}

type aoc202402 struct {
	AOC
}

func (aoc *aoc202402) next(input *Input) ([]int, bool) {
	line, ok := input.Line()
	if !ok {
		return nil, false
	}
	return InputString(line).Ints(), true
}

func (aoc *aoc202402) safe(report []int, dampened int) bool {
	last := report[0]
	start := 1
	if dampened == 0 {
		last = report[1]
		start = 2
	}
	delta := 0
	for i := start; i < len(report); i++ {
		if i == dampened {
			continue
		}
		this := report[i]
		if this > last {
			if delta < 0 || this-last > 3 {
				return false
			}
			delta = 1
			last = this
		} else if this < last {
			if delta > 0 || last-this > 3 {
				return false
			}
			delta = -1
			last = this
		} else {
			return false
		}
	}
	return true
}

func (aoc *aoc202402) Part1(input *Input) string {
	count := 0
	for report, ok := aoc.next(input); ok; report, ok = aoc.next(input) {
		if aoc.safe(report, -1) {
			count++
		}
	}
	return IntResult(count)
}

func (aoc aoc202402) Part2(input *Input) string {
	count := 0
	for report, ok := aoc.next(input); ok; report, ok = aoc.next(input) {
		for dampened := range report {
			if aoc.safe(report, dampened) {
				count++
				break
			}
		}
	}
	return IntResult(count)
}
