package main

import (
	"strings"
)

func init() {
	Register(&aoc202301{
		AOC: AOC{
			Day:           1,
			InputFilename: "../2023/input/01.txt",
			Tests: []Test{
				Test{
					Input: `1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
`,
					Part1: "142",
					Part2: "",
				},
				Test{
					Input: `two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
`,
					Part1: "",
					Part2: "281",
				},
			},
		},
	})
}

type aoc202301 struct {
	AOC
}

func (aoc *aoc202301) digit(line string) (int, bool) {
	switch line[0] {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return int(line[0] - '0'), true
	default:
		return 0, false
	}
}

func (aoc *aoc202301) Part1(input *Input) string {
	result := 0
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		tens := 0
		ones := 0
		for line != "" {
			digit, ok := aoc.digit(line)
			line = line[1:]
			if ok {
				tens = digit
				ones = digit
				break
			}
		}
		for line != "" {
			digit, ok := aoc.digit(line)
			line = line[1:]
			if ok {
				ones = digit
			}
		}
		result += tens*10 + ones
	}
	return IntResult(result)
}

func (aoc *aoc202301) digit2(line string) (int, bool) {
	switch line[0] {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return int(line[0] - '0'), true
	case 'e':
		if strings.HasPrefix(line, "eight") {
			return 8, true
		}
	case 'f':
		if strings.HasPrefix(line, "four") {
			return 4, true
		} else if strings.HasPrefix(line, "five") {
			return 5, true
		}
	case 'n':
		if strings.HasPrefix(line, "nine") {
			return 9, true
		}
	case 'o':
		if strings.HasPrefix(line, "one") {
			return 1, true
		}
	case 's':
		if strings.HasPrefix(line, "six") {
			return 6, true
		} else if strings.HasPrefix(line, "seven") {
			return 7, true
		}
	case 't':
		if strings.HasPrefix(line, "two") {
			return 2, true
		} else if strings.HasPrefix(line, "three") {
			return 3, true
		}
	}
	return 0, false
}

func (aoc *aoc202301) Part2(input *Input) string {
	result := 0
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		tens := 0
		ones := 0
		for line != "" {
			digit, ok := aoc.digit2(line)
			line = line[1:]
			if ok {
				tens = digit
				ones = digit
				break
			}
		}
		for line != "" {
			digit, ok := aoc.digit2(line)
			line = line[1:]
			if ok {
				ones = digit
			}
		}
		result += tens*10 + ones
	}
	return IntResult(result)
}
