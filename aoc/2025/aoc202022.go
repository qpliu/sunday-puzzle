package main

import (
	"container/list"
)

func init() {
	Register(&aoc202022{
		AOC: AOC{
			Day:           22,
			InputFilename: "../2020/input/22.txt",
			Tests: []Test{
				Test{
					Input: `Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10
`,
					Part1: "306",
					Part2: "291",
				},
			},
		},
	})
}

type aoc202022 struct {
	AOC
}

func (aoc *aoc202022) parse(input *Input) (*list.List, *list.List) {
	p1 := &list.List{}
	p2 := &list.List{}

	input.Int()
	for {
		if input.Skip("\n\nPlayer 2:") {
			break
		}
		i, ok := input.Int()
		if !ok {
			panic("bad input")
		}
		p1.PushBack(i)
	}
	for i := range input.IntSeq() {
		p2.PushBack(i)
	}
	return p1, p2
}

func (aoc *aoc202022) score(p1, p2 *list.List) int {
	result := 0
	f := 1
	for e := p1.Back(); e != nil; e = e.Prev() {
		result += f * e.Value.(int)
		f++
	}
	f = 1
	for e := p2.Back(); e != nil; e = e.Prev() {
		result += f * e.Value.(int)
		f++
	}
	return result
}

func (aoc *aoc202022) Part1(input *Input) string {
	p1, p2 := aoc.parse(input)
	for p1.Len() > 0 && p2.Len() > 0 {
		e1 := p1.Front()
		p1.Remove(e1)
		c1 := e1.Value.(int)
		e2 := p2.Front()
		p2.Remove(e2)
		c2 := e2.Value.(int)
		if c1 > c2 {
			p1.PushBack(c1)
			p1.PushBack(c2)
		} else {
			p2.PushBack(c2)
			p2.PushBack(c1)
		}
	}
	return IntResult(aoc.score(p1, p2))
}

func (aoc *aoc202022) game(p1, p2 *list.List) {
	// there are 50 cards in the deck in my input
	history := map[[51]int]bool{}
	for p1.Len() > 0 && p2.Len() > 0 {
		{
			state := [51]int{}
			i := 0
			for e := p1.Front(); e != nil; e = e.Next() {
				state[i] = e.Value.(int)
				i++
			}
			i++
			for e := p2.Front(); e != nil; e = e.Next() {
				state[i] = e.Value.(int)
				i++
			}
			if history[state] {
				p2.Init()
				return
			}
			history[state] = true
		}

		e1 := p1.Front()
		p1.Remove(e1)
		c1 := e1.Value.(int)
		e2 := p2.Front()
		p2.Remove(e2)
		c2 := e2.Value.(int)

		if p1.Len() < c1 || p2.Len() < c2 {
			if c1 > c2 {
				p1.PushBack(c1)
				p1.PushBack(c2)
			} else {
				p2.PushBack(c2)
				p2.PushBack(c1)
			}
			continue
		}

		rp1 := &list.List{}
		{
			e := p1.Front()
			for range c1 {
				rp1.PushBack(e.Value.(int))
				e = e.Next()
			}
		}
		rp2 := &list.List{}
		{
			e := p2.Front()
			for range c2 {
				rp2.PushBack(e.Value.(int))
				e = e.Next()
			}
		}
		aoc.game(rp1, rp2)
		if rp1.Len() > rp2.Len() {
			p1.PushBack(c1)
			p1.PushBack(c2)
		} else {
			p2.PushBack(c2)
			p2.PushBack(c1)
		}
	}
}

func (aoc *aoc202022) Part2(input *Input) string {
	p1, p2 := aoc.parse(input)
	aoc.game(p1, p2)
	return IntResult(aoc.score(p1, p2))
}
