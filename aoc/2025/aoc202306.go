package main

import (
	"math"
)

func init() {
	Register(&aoc202306{
		AOC: AOC{
			Day:           6,
			InputFilename: "../2023/input/06.txt",
			Tests: []Test{
				Test{
					Input: `Time:      7  15   30
Distance:  9  40  200
`,
					Part1: "288",
					Part2: "71503",
				},
			},
		},
	})
}

type aoc202306 struct {
	AOC
}

func (aoc *aoc202306) parse(input *Input) [][2]int {
	numbers := input.Ints()
	races := [][2]int{}
	for i := range len(numbers) / 2 {
		races = append(races, [2]int{numbers[i], numbers[i+len(numbers)/2]})
	}
	return races
}

func (aoc *aoc202306) ways(t, d int) int {
	// d = (t-p)*p
	// p = t/2 +- sqrt(t^2 - 4*d)/2
	disc := math.Sqrt(float64(t*t - 4*d))
	plo := int(math.Ceil((float64(t) - disc) / 2))
	if (t-plo)*plo <= d {
		plo++
	}
	phi := int(math.Floor((float64(t) + disc) / 2))
	if (t-phi)*phi <= d {
		phi--
	}
	return phi - plo + 1
}

func (aoc *aoc202306) Part1(input *Input) string {
	result := 1
	for _, race := range aoc.parse(input) {
		result *= aoc.ways(race[0], race[1])
	}
	return IntResult(result)
}

func (aoc *aoc202306) parse2(input *Input) (int, int) {
	t := 0
	d := 0
	haveT := false
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		switch ch {
		case '\n':
			haveT = true
		case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if !haveT {
				t = t*10 + int(ch-'0')
			} else {
				d = d*10 + int(ch-'0')
			}
		}
	}
	return t, d
}

func (aoc *aoc202306) Part2(input *Input) string {
	return IntResult(aoc.ways(aoc.parse2(input)))
}
