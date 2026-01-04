package main

import (
	"slices"
)

func init() {
	Register(&aoc202225{
		AOC: AOC{
			Day:           25,
			InputFilename: "../2022/input/25.txt",
			Tests: []Test{
				Test{
					Input: `1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
`,
					Part1: "2=-1=0",
					Part2: "",
				},
			},
		},
	})
}

type aoc202225 struct {
	AOC
}

func (aoc *aoc202225) Part1(input *Input) string {
	sum := 0
	for word := range input.Words() {
		n := 0
		for _, ch := range word {
			n *= 5
			switch ch {
			case '=':
				n -= 2
			case '-':
				n -= 1
			case '0':
			case '1':
				n += 1
			case '2':
				n += 2
			default:
				panic("bad input")
			}
		}
		sum += n
	}

	result := []byte{}
	for sum != 0 {
		sum += 2
		switch (sum%5 + 5) % 5 {
		case 0:
			result = append(result, '=')
		case 1:
			result = append(result, '-')
		case 2:
			result = append(result, '0')
		case 3:
			result = append(result, '1')
		case 4:
			result = append(result, '2')
		default:
			panic("?")
		}
		sum /= 5
	}

	slices.Reverse(result)
	return string(result)
}

func (aoc *aoc202225) Part2(input *Input) string {
	return ""
}
