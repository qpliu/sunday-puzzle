package main

import (
	"slices"
)

func init() {
	Register(&aoc202311{
		AOC: AOC{
			Day:           11,
			InputFilename: "../2023/input/11.txt",
			Tests: []Test{
				Test{
					Input: `...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
`,
					Part1: "374",
					Part2: "8410",
				},
			},
		},
	})
}

type aoc202311 struct {
	AOC
}

func (aoc *aoc202311) result(expansion int, input *Input) string {
	xys := [][2]int{}
	xmap := map[int]int{}
	ymap := map[int]int{}
	x := 0
	y := 0
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		if ch == '\n' {
			y++
			x = 0
			continue
		}
		if ch == '#' {
			xys = append(xys, [2]int{x, y})
			xmap[x] = x
			ymap[y] = y
		}
		x++
	}

	xs := []int{}
	for x := range xmap {
		xs = append(xs, x)
	}
	slices.Sort(xs)
	dx := 0
	x = xs[0]
	for _, x2 := range xs[1:] {
		if x2 > x+1 {
			dx += (expansion - 1) * (x2 - x - 1)
		}
		xmap[x2] = x2 + dx
		x = x2
	}

	ys := []int{}
	for y := range ymap {
		ys = append(ys, y)
	}
	slices.Sort(ys)
	dy := 0
	y = ys[0]
	for _, y2 := range ys[1:] {
		if y2 > y+1 {
			dy += (expansion - 1) * (y2 - y - 1)
		}
		ymap[y2] = y2 + dy
		y = y2
	}

	result := 0
	for i, xy1 := range xys {
		x1 := xmap[xy1[0]]
		y1 := ymap[xy1[1]]
		for _, xy2 := range xys[:i] {
			x2 := xmap[xy2[0]]
			y2 := ymap[xy2[1]]
			result += max(x1-x2, x2-x1) + max(y1-y2, y2-y1)
		}
	}
	return IntResult(result)
}

func (aoc *aoc202311) Part1(input *Input) string {
	return aoc.result(2, input)
}

func (aoc *aoc202311) Test2(input *Input) string {
	return aoc.result(100, input)
}

func (aoc *aoc202311) Part2(input *Input) string {
	return aoc.result(1000000, input)
}
