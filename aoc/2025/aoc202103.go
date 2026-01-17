package main

func init() {
	Register(&aoc202103{
		AOC: AOC{
			Day:           3,
			InputFilename: "../2021/input/03.txt",
			Tests: []Test{
				Test{
					Input: `00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
`,
					Part1: "198",
					Part2: "230",
				},
			},
		},
	})
}

type aoc202103 struct {
	AOC
}

func (aoc *aoc202103) Part1(input *Input) string {
	line, ok := input.Line()
	ones := make([]int, len(line))
	n := 0
	for ok {
		n++
		for i, b := range line {
			if b == '1' {
				ones[i]++
			}
		}
		line, ok = input.Line()
	}
	gamma, epsilon := 0, 0
	for _, b := range ones {
		gamma <<= 1
		epsilon <<= 1
		if b > n-b {
			gamma |= 1
		} else {
			epsilon |= 1
		}
	}
	return IntResult(gamma * epsilon)
}

func (aoc *aoc202103) Part2(input *Input) string {
	reports := []string{}
	for report := range input.Lines() {
		reports = append(reports, report)
	}

	rating := func(criterion bool) int {
		r1 := make([]string, len(reports))
		copy(r1, reports)
		r2 := make([]string, 0, len(reports))
		bit := 0
		for {
			n := len(r1)
			ones := 0
			for _, r := range r1 {
				if r[bit] == '1' {
					ones++
				}
			}
			keep := byte('0')
			if (ones >= n-ones) == criterion {
				keep = '1'
			}
			for _, r := range r1 {
				if r[bit] == keep {
					r2 = append(r2, r)
				}
			}

			if len(r2) == 1 {
				rating := 0
				for _, b := range r2[0] {
					rating <<= 1
					if b == '1' {
						rating |= 1
					}
				}
				return rating
			}
			r1, r2 = r2, r1[:0]
			bit++
		}
	}
	return IntResult(rating(true) * rating(false))
}
