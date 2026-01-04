package main

import (
	"container/list"
)

func init() {
	Register(&aoc202220{
		AOC: AOC{
			Day:           20,
			InputFilename: "../2022/input/20.txt",
			Tests: []Test{
				Test{
					Input: `1
2
-3
3
-2
0
4
`,
					Part1: "3",
					Part2: "1623178306",
				},
			},
		},
	})
}

type aoc202220 struct {
	AOC
}

func (aoc *aoc202220) parse(key int, input *Input) (*list.Element, []*list.Element, *list.List, int) {
	size := 0
	zero := (*list.Element)(nil)
	elements := []*list.Element{}
	numbers := &list.List{}

	for i, ok := input.Int(); ok; i, ok = input.Int() {
		size++
		e := numbers.PushBack(i * key)
		if i == 0 {
			zero = e
		}
		elements = append(elements, e)
	}
	return zero, elements, numbers, size
}

func (aoc *aoc202220) mix(elements []*list.Element, numbers *list.List, size int) {
	for _, e := range elements {
		n := ((e.Value.(int) % (size - 1)) + (size - 1)) % (size - 1)
		mark := e
		if n < size-1-n {
			for range n {
				mark = mark.Next()
				if mark == nil {
					mark = numbers.Front()
				}
			}
			numbers.MoveAfter(e, mark)
		} else {
			for range size - 1 - n {
				mark = mark.Prev()
				if mark == nil {
					mark = numbers.Back()
				}
			}
			numbers.MoveBefore(e, mark)
		}
	}
}

func (aoc *aoc202220) result(key, mixes int, input *Input) string {
	zero, elements, numbers, size := aoc.parse(key, input)
	for range mixes {
		aoc.mix(elements, numbers, size)
	}

	result := 0
	e := zero
	for range 3 {
		for range 1000 {
			e = e.Next()
			if e == nil {
				e = numbers.Front()
			}
		}
		result += e.Value.(int)
	}
	return IntResult(result)
}

func (aoc *aoc202220) Part1(input *Input) string {
	return aoc.result(1, 1, input)
}

func (aoc *aoc202220) Part2(input *Input) string {
	return aoc.result(811589153, 10, input)
}
