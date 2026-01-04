package main

import (
	"bytes"
	"container/list"
)

func init() {
	Register(&aoc202205{
		AOC: AOC{
			Day:           5,
			InputFilename: "../2022/input/05.txt",
			Tests: []Test{
				Test{
					Input: `    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
`,
					Part1: "CMZ",
					Part2: "MCD",
				},
			},
		},
	})
}

type aoc202205 struct {
	AOC
}

func (aoc *aoc202205) parse(input *Input) []*list.List {
	stacks := []*list.List{}
	i := 0
	for {
		if input.Skip(" 1  ") {
			input.Line()
			return stacks
		} else if input.Skip("\n") {
			i = 0
		} else if input.Skip("    ") {
			if len(stacks) == i {
				stacks = append(stacks, list.New())
			}
			i++
		} else if input.Skip("   \n") {
			if len(stacks) == i {
				stacks = append(stacks, list.New())
			}
			i = 0
		} else if input.Skip("[") {
			ch, _ := input.Char()
			if len(stacks) == i {
				stacks = append(stacks, list.New())
			}
			stacks[i].PushBack(ch)
			if input.Skip("]\n") {
				i = 0
			} else if input.Skip("] ") {
				i++
			} else {
				panic("bad input")
			}
		} else {
			panic("bad input")
		}
	}
}

func (aoc *aoc202205) next(input *Input) (int, int, int, bool) {
	move, _ := input.Int()
	from, _ := input.Int()
	to, ok := input.Int()
	return move, from - 1, to - 1, ok
}

func (aoc *aoc202205) tops(stacks []*list.List) string {
	buf := bytes.Buffer{}
	for _, stack := range stacks {
		buf.WriteByte(stack.Front().Value.(byte))
	}
	return buf.String()
}

func (aoc *aoc202205) Part1(input *Input) string {
	stacks := aoc.parse(input)
	for move, from, to, ok := aoc.next(input); ok; move, from, to, ok = aoc.next(input) {
		for range move {
			stacks[to].PushFront(stacks[from].Remove(stacks[from].Front()))
		}
	}
	return aoc.tops(stacks)
}

func (aoc *aoc202205) Part2(input *Input) string {
	stacks := aoc.parse(input)
	crane := list.New()
	for move, from, to, ok := aoc.next(input); ok; move, from, to, ok = aoc.next(input) {
		for range move {
			crane.PushFront(stacks[from].Remove(stacks[from].Front()))
		}
		for range move {
			stacks[to].PushFront(crane.Remove(crane.Front()))
		}
	}
	return aoc.tops(stacks)
}
