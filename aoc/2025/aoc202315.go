package main

import (
	"bytes"
)

func init() {
	Register(&aoc202315{
		AOC: AOC{
			Day:           15,
			InputFilename: "../2023/input/15.txt",
			Tests: []Test{
				Test{
					Input: `HASH
`,
					Part1: "52",
					Part2: "",
				},
				Test{
					Input: `rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
`,
					Part1: "1320",
					Part2: "145",
				},
			},
		},
	})
}

type aoc202315 struct {
	AOC
}

func (aoc *aoc202315) Part1(input *Input) string {
	result := 0
	hash := 0
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		switch ch {
		case '\n':
		case ',':
			result += hash
			hash = 0
		default:
			hash = ((hash + int(ch)) * 17) % 256
		}
	}
	result += hash
	return IntResult(result)
}

func (aoc *aoc202315) next(input *Input) (string, int, bool) {
	// My input only has 1 digit focal lengths, always greater than 0.
	buf := bytes.Buffer{}
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		switch ch {
		case ',':
		case '-':
			return buf.String(), -1, true
		case '=':
			ch, _ = input.Char()
			return buf.String(), int(ch - '0'), true
		default:
			buf.WriteByte(ch)
		}
	}
	return "", 0, false
}

func (aoc *aoc202315) hash(s string) int {
	h := 0
	for _, ch := range []byte(s) {
		h = ((h + int(ch)) * 17) % 256
	}
	return h
}

func (aoc *aoc202315) Part2(input *Input) string {
	boxes := [256][]struct {
		lens string
		flen int
	}{}
oploop:
	for lens, op, ok := aoc.next(input); ok; lens, op, ok = aoc.next(input) {
		i := aoc.hash(lens)
		for j := range boxes[i] {
			if boxes[i][j].lens == lens {
				if op > 0 {
					boxes[i][j].flen = op
				} else {
					copy(boxes[i][j:], boxes[i][j+1:])
					boxes[i] = boxes[i][:len(boxes[i])-1]
				}
				continue oploop
			}
		}
		if op > 0 {
			boxes[i] = append(boxes[i], struct {
				lens string
				flen int
			}{lens, op})
		}
	}

	result := 0
	for i, box := range boxes {
		for j, lens := range box {
			result += (i + 1) * (j + 1) * lens.flen
		}
	}
	return IntResult(result)
}
