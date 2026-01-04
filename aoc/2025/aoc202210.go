package main

import (
	"bytes"
)

func init() {
	Register(&aoc202210{
		AOC: AOC{
			Day:           10,
			InputFilename: "../2022/input/10.txt",
			Tests: []Test{
				Test{
					Input: `addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
`,
					Part1: "13140",
					Part2: "",
				},
			},
		},
	})
}

type aoc202210 struct {
	AOC
}

func (aoc *aoc202210) next(input *Input) (int, bool) {
	word, ok := input.Word()
	if !ok {
		return 0, false
	}
	if word == "noop" {
		return 0, true
	}
	return input.Int()
}

func (aoc *aoc202210) Part1(input *Input) string {
	cycles := []int{20, 60, 100, 140, 180, 220}
	cycle := 1
	x := 1

	result := 0
	for n, ok := aoc.next(input); ok; n, ok = aoc.next(input) {
		if n == 0 {
			cycle++
		} else {
			cycle += 2
		}
		if cycle > cycles[0] {
			result += x * cycles[0]
			cycles = cycles[1:]
			if len(cycles) == 0 {
				return IntResult(result)
			}
		}
		x += n
	}
	panic("bad input")
}

func (aoc *aoc202210) Part2(input *Input) string {
	x := 1
	n := 0
	latency := 0
	pixels := []string{}
	for range 6 {
		buf := bytes.Buffer{}
		for px := range 40 {
			if latency > 0 {
				latency--
			} else {
				x += n
				n, _ = aoc.next(input)
				if n != 0 {
					latency = 1
				}
			}
			if px-x < -1 || px-x > 1 {
				buf.WriteByte('.')
			} else {
				buf.WriteByte('#')
			}
		}
		pixels = append(pixels, buf.String())
	}
	return OCR4x6(pixels)
}
