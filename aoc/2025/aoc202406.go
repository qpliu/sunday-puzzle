package main

import (
	"runtime"
)

func init() {
	Register(&aoc202406{
		AOC: AOC{
			Day:           6,
			InputFilename: "../2024/input/06.txt",
			Tests: []Test{
				Test{
					Input: `....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
`,
					Part1: "41",
					Part2: "6",
				},
			},
		},
	})
}

type aoc202406 struct {
	AOC
}

func (aoc *aoc202406) parse(input *Input) ([2]int, map[[2]int]byte) {
	_, _, grid := input.Grid()
	for xy, ch := range grid {
		if ch == '^' {
			return xy, grid
		}
	}
	panic("bad input")
}

func (aoc *aoc202406) Part1(input *Input) string {
	xy, grid := aoc.parse(input)
	dxy := [2]int{0, -1}

	result := 1
	grid[xy] = 'X'
	for {
		step := [2]int{xy[0] + dxy[0], xy[1] + dxy[1]}
		if grid[step] == '#' {
			dxy = [2]int{-dxy[1], dxy[0]}
			continue
		}
		ch, ok := grid[step]
		if !ok {
			return IntResult(result)
		}
		if ch != 'X' {
			grid[step] = 'X'
			result++
		}
		xy = step
	}
}

func (aoc aoc202406) hasLoop(xy, dxy [2]int, grid map[[2]int]byte) bool {
	path := map[[2][2]int]bool{}
	block := [2]int{xy[0] + dxy[0], xy[1] + dxy[1]}

	for {
		step := [2]int{xy[0] + dxy[0], xy[1] + dxy[1]}
		if path[[2][2]int{xy, dxy}] {
			return true
		}
		path[[2][2]int{xy, dxy}] = true
		if step == block || grid[step] == '#' {
			dxy = [2]int{-dxy[1], dxy[0]}
			continue
		}
		if _, ok := grid[step]; !ok {
			return false
		}
		xy = step
	}
}

func (aoc aoc202406) Part2(input *Input) string {
	xy, grid := aoc.parse(input)
	dxy := [2]int{0, -1}

	work := make(chan [2][2]int)
	results := make(chan int)
	for _ = range runtime.NumCPU() {
		go func() {
			total := 0
			for {
				xydxy, ok := <-work
				if !ok {
					results <- total
					return
				}
				if aoc.hasLoop(xydxy[0], xydxy[1], grid) {
					total++
				}
			}
		}()
	}

	checked := map[[2]int]bool{}
	for {
		step := [2]int{xy[0] + dxy[0], xy[1] + dxy[1]}
		if grid[step] == '#' {
			dxy = [2]int{-dxy[1], dxy[0]}
			continue
		}
		if _, ok := grid[step]; !ok {
			break
		}
		if !checked[step] {
			checked[step] = true
			work <- [2][2]int{xy, dxy}
		}
		xy = step
	}
	close(work)
	result := 0
	for _ = range runtime.NumCPU() {
		result += <-results
	}
	return IntResult(result)
}
