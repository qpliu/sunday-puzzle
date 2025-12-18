package main

import (
	"unicode"
)

func init() {
	Register(&aoc202403{
		AOC: AOC{
			Day:           3,
			InputFilename: "../2024/input/03.txt",
			Tests: []Test{
				Test{
					Input: `xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
`,
					Part1: "161",
					Part2: "",
				},
				Test{
					Input: `xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
`,
					Part1: "",
					Part2: "48",
				},
			},
		},
	})
}

type aoc202403 struct {
	AOC
}

func (aoc *aoc202403) num(input *Input) (int, bool) {
	ch, ok := input.Peek()
	if !ok || !unicode.IsDigit(rune(ch)) {
		return 0, false
	}
	n := 0
	for {
		n = n*10 + int(ch-'0')
		input.Char()
		ch, ok = input.Peek()
		if !ok || !unicode.IsDigit(rune(ch)) {
			break
		}
	}
	return n, true

}

func (aoc *aoc202403) next1(input *Input) ([2]int, bool) {
	for {
		for {
			ch, ok := input.Char()
			if !ok {
				return [2]int{}, false
			}
			if ch == 'm' && input.Skip("ul(") {
				break
			}
		}
		n1, ok := aoc.num(input)
		if !ok {
			continue
		}
		if ch, ok := input.Peek(); !ok || ch != ',' {
			continue
		}
		input.Char()
		n2, ok := aoc.num(input)
		if !ok {
			continue
		}
		if ch, ok := input.Peek(); !ok || ch != ')' {
			continue
		}
		return [2]int{n1, n2}, true
	}
}

func (aoc *aoc202403) Part1(input *Input) string {
	result := 0
	for pair, ok := aoc.next1(input); ok; pair, ok = aoc.next1(input) {
		result += pair[0] * pair[1]
	}
	return IntResult(result)
}

func (aoc *aoc202403) toDo(input *Input) {
	for {
		ch, ok := input.Char()
		if !ok {
			return
		}
		if ch == 'd' && input.Skip("o()") {
			return
		}
	}
}

func (aoc *aoc202403) next2(input *Input) ([2]int, bool) {
	for {
		for {
			ch, ok := input.Char()
			if !ok {
				return [2]int{}, false
			}
			if ch == 'm' && input.Skip("ul(") {
				break
			} else if ch == 'd' && input.Skip("on't()") {
				aoc.toDo(input)
			}
		}
		n1, ok := aoc.num(input)
		if !ok {
			continue
		}
		if ch, ok := input.Peek(); !ok || ch != ',' {
			continue
		}
		input.Char()
		n2, ok := aoc.num(input)
		if !ok {
			continue
		}
		if ch, ok := input.Peek(); !ok || ch != ')' {
			continue
		}
		return [2]int{n1, n2}, true
	}
}

func (aoc aoc202403) Part2(input *Input) string {
	result := 0
	for pair, ok := aoc.next2(input); ok; pair, ok = aoc.next2(input) {
		result += pair[0] * pair[1]
	}
	return IntResult(result)
}
