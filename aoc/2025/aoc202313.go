package main

import (
	"runtime"
)

func init() {
	Register(&aoc202313{
		AOC: AOC{
			Day:           13,
			InputFilename: "../2023/input/13.txt",
			Tests: []Test{
				Test{
					Input: `#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
`,
					Part1: "405",
					Part2: "400",
				},
			},
		},
	})
}

type aoc202313 struct {
	AOC
}

func (aoc *aoc202313) next(input *Input) (int, int, map[[2]int]byte, bool) {
	paragraph, ok := input.Paragraph()
	if !ok {
		return 0, 0, nil, false
	}
	w, h, grid := InputString(paragraph).Grid()
	return w, h, grid, true
}

func (aoc *aoc202313) reflection(smudge, w, h int, grid map[[2]int]byte) int {
	for x := range w - 1 {
		nerrors := 0
		for y := range h {
			for dx := range w {
				if x-dx < 0 || x+1+dx >= w {
					break
				}
				if grid[[2]int{x - dx, y}] != grid[[2]int{x + 1 + dx, y}] {
					nerrors++
					if nerrors > smudge {
						break
					}
				}
			}
			if nerrors > smudge {
				break
			}
		}
		if nerrors == smudge {
			return x + 1
		}
	}
	for y := range h - 1 {
		nerrors := 0
		for x := range w {
			for dy := range h {
				if y-dy < 0 || y+1+dy >= h {
					break
				}
				if grid[[2]int{x, y - dy}] != grid[[2]int{x, y + 1 + dy}] {
					nerrors++
					if nerrors > smudge {
						break
					}
				}
			}
			if nerrors > smudge {
				break
			}
		}
		if nerrors == smudge {
			return 100 * (y + 1)
		}
	}
	panic("bad input")
}

func (aoc *aoc202313) result(smudge int, input *Input) string {
	in := make(chan struct {
		w, h int
		grid map[[2]int]byte
	})
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for grid := range in {
				result += aoc.reflection(smudge, grid.w, grid.h, grid.grid)
			}
			out <- result
		}()
	}
	for w, h, grid, ok := aoc.next(input); ok; w, h, grid, ok = aoc.next(input) {
		in <- struct {
			w, h int
			grid map[[2]int]byte
		}{w, h, grid}
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202313) Part1(input *Input) string {
	return aoc.result(0, input)
}

func (aoc *aoc202313) Part2(input *Input) string {
	return aoc.result(1, input)
}
