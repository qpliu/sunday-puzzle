package main

func init() {
	Register(&aoc202203{
		AOC: AOC{
			Day:           3,
			InputFilename: "../2022/input/03.txt",
			Tests: []Test{
				Test{
					Input: `vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
`,
					Part1: "157",
					Part2: "70",
				},
			},
		},
	})
}

type aoc202203 struct {
	AOC
}

func (aoc *aoc202203) c2p(ch byte) int {
	if ch >= 'a' {
		return int(ch - 'a' + 1)
	} else {
		return int(ch - 'A' + 27)
	}
}

func (aoc *aoc202203) s2p(set uint64) int {
	for i := range 64 {
		if set == 1 {
			return i
		}
		set >>= 1
	}
	panic("bad input")
}

func (aoc *aoc202203) Part1(input *Input) string {
	result := 0
	for line := range input.Lines() {
		n := len(line)
		var a, b uint64
		for i, l2 := range []byte(line[n/2:]) {
			a |= 1 << aoc.c2p(line[i])
			b |= 1 << aoc.c2p(l2)
		}
		result += aoc.s2p(a & b)
	}
	return IntResult(result)
}

func (aoc *aoc202203) next(input *Input) (uint64, bool) {
	line, ok := input.Line()
	if !ok {
		return 0, false
	}
	var sack uint64
	for _, ch := range []byte(line) {
		sack |= 1 << aoc.c2p(ch)
	}
	return sack, true
}

func (aoc *aoc202203) Part2(input *Input) string {
	result := 0
	for {
		a, _ := aoc.next(input)
		b, _ := aoc.next(input)
		c, ok := aoc.next(input)
		if !ok {
			break
		}
		result += aoc.s2p(a & b & c)
	}
	return IntResult(result)
}
