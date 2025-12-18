package main

import (
	"bytes"
)

func init() {
	Register(&aoc202417{
		AOC: AOC{
			Day:           17,
			InputFilename: "../2024/input/17.txt",
			Tests: []Test{
				Test{
					Input: `Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
`,
					Part1: "4,6,3,5,6,3,5,2,1,0",
					Part2: "",
				},
				Test{
					Input: `Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
`,
					Part1: "",
					Part2: "117440",
				},
			},
		},
	})
}

type aoc202417 struct {
	AOC
}

func (aoc *aoc202417) parse(input *Input) (int, int, int, []int) {
	a, _ := input.Int()
	b, _ := input.Int()
	c, _ := input.Int()
	program := input.Ints()
	return a, b, c, program
}

func (aoc *aoc202417) run(a, b, c int, program []int) []int {
	combo := func(i int) int {
		switch program[i] {
		case 0:
			return 0
		case 1:
			return 1
		case 2:
			return 2
		case 3:
			return 3
		case 4:
			return a
		case 5:
			return b
		case 6:
			return c
		default:
			panic("bad input")
		}
	}

	out := []int{}
	ip := 0
	for ip >= 0 && ip < len(program)-1 {
		switch program[ip] {
		case 0:
			a >>= combo(ip + 1)
			ip += 2
		case 1:
			b ^= program[ip+1]
			ip += 2
		case 2:
			b = combo(ip+1) & 7
			ip += 2
		case 3:
			if a == 0 {
				ip += 2
			} else {
				ip = combo(ip + 1)
			}
		case 4:
			b ^= c
			ip += 2
		case 5:
			out = append(out, combo(ip+1)&7)
			ip += 2
		case 6:
			b = a >> combo(ip+1)
			ip += 2
		case 7:
			c = a >> combo(ip+1)
			ip += 2
		default:
			panic("bad input")
		}
	}
	return out
}

func (aoc *aoc202417) Part1(input *Input) string {
	out := aoc.run(aoc.parse(input))
	buf := &bytes.Buffer{}
	for _, b := range out {
		buf.WriteString(IntResult(b))
		buf.WriteByte(',')
	}
	buf.Truncate(buf.Len() - 1)
	return buf.String()
}

func (aoc *aoc202417) Part2(input *Input) string {
	_, b, c, program := aoc.parse(input)
	// based on disassembling input, program
	// outputs three bits at a time, so try
	// to match a to desired output three bits
	// at a time
	as := []int{0}
	for i := range program {
		p := program[len(program)-1-i]
		nexts := []int{}
		for _, a := range as {
			for n := range 8 {
				out := aoc.run(a+n, b, c, program)
				if len(out) > i && out[0] == p {
					nexts = append(nexts, (a+n)<<3)
				}
			}
		}
		as = nexts
	}
	return IntResult(as[0] >> 3)
}
