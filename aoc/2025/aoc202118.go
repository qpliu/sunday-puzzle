package main

import (
	"runtime"
)

func init() {
	Register(&aoc202118{
		AOC: AOC{
			Day:           18,
			InputFilename: "../2021/input/18.txt",
			Tests: []Test{
				Test{
					Input: `[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]
`,
					Part1: "4140",
					Part2: "3993",
				},
			},
		},
	})
}

type aoc202118 struct {
	AOC
}

type aoc202118_number struct {
	number      int
	depth       int
	left, right *aoc202118_number
}

func (number aoc202118_number) magnitude() int {
	if number.depth == 0 {
		return number.number
	}
	return 3*number.left.magnitude() + 2*number.right.magnitude()
}

func (number *aoc202118_number) reduce() *aoc202118_number {
	for {
		for {
			_, _, newNumber := number.explode(4)
			if newNumber == nil {
				break
			}
			number = newNumber
		}
		newNumber := number.split()
		if newNumber == nil {
			return number
		}
		number = newNumber
	}
}

func (number *aoc202118_number) explode(maxDepth int) (int, int, *aoc202118_number) {
	if number.depth <= maxDepth {
		return 0, 0, nil
	}
	if number.depth == 1 {
		return number.left.number, number.right.number, &aoc202118_number{}
	}
	left, right, newLeft := number.left.explode(maxDepth - 1)
	if newLeft != nil {
		if right == 0 {
			return left, 0, &aoc202118_number{
				depth: max(newLeft.depth, number.right.depth) + 1,
				left:  newLeft,
				right: number.right,
			}
		}
		newRight := number.right.explodeRight(right)
		return left, 0, &aoc202118_number{
			depth: max(newLeft.depth, newRight.depth) + 1,
			left:  newLeft,
			right: newRight,
		}
	}
	left, right, newRight := number.right.explode(maxDepth - 1)
	if newRight != nil {
		if left == 0 {
			return 0, right, &aoc202118_number{
				depth: max(number.left.depth, newRight.depth) + 1,
				left:  number.left,
				right: newRight,
			}
		}
		newLeft := number.left.explodeLeft(left)
		return 0, right, &aoc202118_number{
			depth: max(newLeft.depth, newRight.depth) + 1,
			left:  newLeft,
			right: newRight,
		}
	}
	panic("?")
}

func (number *aoc202118_number) explodeRight(n int) *aoc202118_number {
	if number.depth == 0 {
		return &aoc202118_number{number: number.number + n}
	}
	newLeft := number.left.explodeRight(n)
	return &aoc202118_number{
		depth: number.depth,
		left:  newLeft,
		right: number.right,
	}
}

func (number *aoc202118_number) explodeLeft(n int) *aoc202118_number {
	if number.depth == 0 {
		return &aoc202118_number{number: number.number + n}
	}
	newRight := number.right.explodeLeft(n)
	return &aoc202118_number{
		depth: number.depth,
		left:  number.left,
		right: newRight,
	}
}

func (number *aoc202118_number) split() *aoc202118_number {
	if number.depth == 0 {
		if number.number < 10 {
			return nil
		}
		return &aoc202118_number{
			depth: 1,
			left:  &aoc202118_number{number: number.number / 2},
			right: &aoc202118_number{number: (number.number + 1) / 2},
		}
	}
	newLeft := number.left.split()
	if newLeft != nil {
		return &aoc202118_number{
			depth: max(newLeft.depth, number.right.depth) + 1,
			left:  newLeft,
			right: number.right,
		}
	}
	newRight := number.right.split()
	if newRight != nil {
		return &aoc202118_number{
			depth: max(number.left.depth, newRight.depth) + 1,
			left:  number.left,
			right: newRight,
		}
	}
	return nil
}

func (aoc *aoc202118) parseNumber(input *Input) (*aoc202118_number, bool) {
	if !input.Skip("[") {
		n, ok := input.Int()
		if !ok {
			return nil, false
		}
		return &aoc202118_number{number: n}, true
	}
	left, ok := aoc.parseNumber(input)
	if !ok {
		panic("bad input")
	}
	if !input.Skip(",") {
		panic("bad input")
	}
	right, ok := aoc.parseNumber(input)
	if !ok {
		panic("bad input")
	}
	if !input.Skip("]") {
		panic("bad input")
	}
	return &aoc202118_number{
		depth: max(left.depth, right.depth) + 1,
		left:  left,
		right: right,
	}, true
}

func (aoc *aoc202118) parse(input *Input) []*aoc202118_number {
	numbers := []*aoc202118_number{}
	for {
		for input.Skip("\n") {
		}
		number, ok := aoc.parseNumber(input)
		if !ok {
			return numbers
		}
		numbers = append(numbers, number)
	}
}

func (aoc *aoc202118) add(a, b *aoc202118_number) *aoc202118_number {
	n := &aoc202118_number{
		depth: max(a.depth, b.depth) + 1,
		left:  a,
		right: b,
	}
	return n.reduce()
}

func (aoc *aoc202118) Part1(input *Input) string {
	numbers := aoc.parse(input)
	sum := numbers[0]
	for _, number := range numbers[1:] {
		sum = aoc.add(sum, number)
	}
	return IntResult(sum.magnitude())
}

func (aoc *aoc202118) Part2(input *Input) string {
	in := make(chan [2]*aoc202118_number)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for numbers := range in {
				result = max(result, aoc.add(numbers[0], numbers[1]).magnitude())
			}
			out <- result
		}()
	}
	numbers := aoc.parse(input)
	for _, a := range numbers {
		for _, b := range numbers {
			if a == b {
				continue
			}
			in <- [2]*aoc202118_number{a, b}
		}
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result = max(result, <-out)
	}
	return IntResult(result)
}
