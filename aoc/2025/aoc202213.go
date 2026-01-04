package main

import (
	"slices"
)

func init() {
	Register(&aoc202213{
		AOC: AOC{
			Day:           13,
			InputFilename: "../2022/input/13.txt",
			Tests: []Test{
				Test{
					Input: `[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
`,
					Part1: "13",
					Part2: "140",
				},
			},
		},
	})
}

type aoc202213 struct {
	AOC
}

func (aoc *aoc202213) parse(str string) (any, string) {
	switch str[0] {
	case '[':
		val := []any{}
		str = str[1:]
		if str[0] == ']' {
			return val, str[1:]
		}
		for {
			var elt any
			elt, str = aoc.parse(str)
			val = append(val, elt)
			switch str[0] {
			case ',':
				str = str[1:]
			case ']':
				return val, str[1:]
			default:
				panic("bad input")
			}
		}
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		val := int(str[0]) - '0'
		str = str[1:]
		for {
			switch str[0] {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
				val = val*10 + int(str[0]) - '0'
				str = str[1:]
			default:
				return val, str
			}
		}
	default:
		panic("bad input")
	}
}

func (aoc *aoc202213) next(input *Input) (any, bool) {
	line := ""
	for line == "" {
		ok := true
		line, ok = input.Line()
		if !ok {
			return nil, false
		}
	}
	packet, _ := aoc.parse(line)
	return packet, true
}

func (aoc *aoc202213) cmp(aany, bany any) int {
	switch a := aany.(type) {
	case int:
		switch b := bany.(type) {
		case int:
			return a - b
		case []any:
			if len(b) == 0 {
				return 1
			}
			return aoc.cmp([]any{a}, b)
		default:
			panic("?")
		}
	case []any:
		switch b := bany.(type) {
		case int:
			if len(a) == 0 {
				return -1
			}
			return aoc.cmp(a, []any{b})
		case []any:
			for {
				if len(a) == 0 {
					if len(b) == 0 {
						return 0
					}
					return -1
				} else if len(b) == 0 {
					return 1
				}
				cmp := aoc.cmp(a[0], b[0])
				if cmp != 0 {
					return cmp
				}
				a, b = a[1:], b[1:]
			}
		default:
			panic("?")
		}
	default:
		panic("?")
	}
}

func (aoc *aoc202213) Part1(input *Input) string {
	result := 0
	i := 1
	for {
		a, _ := aoc.next(input)
		b, ok := aoc.next(input)
		if !ok {
			return IntResult(result)
		}
		if aoc.cmp(a, b) < 0 {
			result += i
		}
		i++
	}
}

func (aoc *aoc202213) Part2(input *Input) string {
	d2 := []any{[]any{2}}
	d6 := []any{[]any{6}}
	packets := []any{d2, d6}
	for packet, ok := aoc.next(input); ok; packet, ok = aoc.next(input) {
		packets = append(packets, packet)
	}
	slices.SortFunc(packets, aoc.cmp)

	result := 1
	for i, packet := range packets {
		if aoc.cmp(packet, d2) == 0 || aoc.cmp(packet, d6) == 0 {
			result *= i + 1
		}
	}
	return IntResult(result)
}
