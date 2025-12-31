package main

import (
	"runtime"
)

func init() {
	Register(&aoc202310{
		AOC: AOC{
			Day:           10,
			InputFilename: "../2023/input/10.txt",
			Tests: []Test{
				Test{
					Input: `.....
.S-7.
.|.|.
.L-J.
.....
`,
					Part1: "4",
					Part2: "",
				},
				Test{
					Input: `..F7.
.FJ|.
SJ.L7
|F--J
LJ...
`,
					Part1: "8",
					Part2: "",
				},
				Test{
					Input: `...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
`,
					Part1: "",
					Part2: "4",
				},
				Test{
					Input: `..........
.S------7.
.|F----7|.
.||OOOO||.
.||OOOO||.
.|L-7F-J|.
.|II||II|.
.L--JL--J.
..........
`,
					Part1: "",
					Part2: "4",
				},
				Test{
					Input: `.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
`,
					Part1: "",
					Part2: "8",
				},
				Test{
					Input: `FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
`,
					Part1: "",
					Part2: "10",
				},
			},
		},
	})
}

type aoc202310 struct {
	AOC
}

func (aoc *aoc202310) start(grid map[[2]int]byte) ([2]int, [2]int, [2]int) {
	for xy, ch := range grid {
		if ch != 'S' {
			continue
		}
		dxy1 := [2]int{}
		dxy2 := [2]int{}
		rdxy := []*[2]int{&dxy1, &dxy2}
		switch grid[[2]int{xy[0], xy[1] - 1}] {
		case '7', '|', 'F':
			(*(rdxy[0]))[1] = -1
			rdxy = rdxy[1:]
		}
		switch grid[[2]int{xy[0] + 1, xy[1]}] {
		case 'J', '-', '7':
			(*(rdxy[0]))[0] = 1
			rdxy = rdxy[1:]
		}
		switch grid[[2]int{xy[0], xy[1] + 1}] {
		case 'J', '|', 'L':
			(*(rdxy[0]))[1] = 1
			rdxy = rdxy[1:]
		}
		switch grid[[2]int{xy[0] - 1, xy[1]}] {
		case 'F', '-', 'L':
			(*(rdxy[0]))[0] = -1
			rdxy = rdxy[1:]
		}
		return xy, dxy1, dxy2
	}
	panic("bad input")
}

func (aoc *aoc202310) step(xy, dxy [2]int, grid map[[2]int]byte) ([2]int, [2]int) {
	xy[0] += dxy[0]
	xy[1] += dxy[1]
	switch grid[xy] {
	case '-':
		return xy, dxy
	case '|':
		return xy, dxy
	case 'F':
		if dxy == [2]int{-1, 0} {
			return xy, [2]int{0, 1}
		} else {
			return xy, [2]int{1, 0}
		}
	case '7':
		if dxy == [2]int{1, 0} {
			return xy, [2]int{0, 1}
		} else {
			return xy, [2]int{-1, 0}
		}
	case 'L':
		if dxy == [2]int{-1, 0} {
			return xy, [2]int{0, -1}
		} else {
			return xy, [2]int{1, 0}
		}
	case 'J':
		if dxy == [2]int{1, 0} {
			return xy, [2]int{0, -1}
		} else {
			return xy, [2]int{-1, 0}
		}
	case 'S':
		return xy, [2]int{}
	}
	panic("bad input")
}

func (aoc *aoc202310) Part1(input *Input) string {
	_, _, grid := input.Grid()
	xy, dxy1, dxy2 := aoc.start(grid)
	xy1 := xy
	xy2 := xy

	steps := 0
	for {
		xy1, dxy1 = aoc.step(xy1, dxy1, grid)
		if xy1 == xy2 {
			break
		}
		steps++
		xy2, dxy2 = aoc.step(xy2, dxy2, grid)
		if xy1 == xy2 {
			break
		}
	}
	return IntResult(steps)
}

func (aoc *aoc202310) Part2(input *Input) string {
	w, h, grid := input.Grid()
	loop := map[[2]int]byte{}
	{
		start, dxy1, dxy2 := aoc.start(grid)
		switch [4]int{dxy1[0], dxy1[1], dxy2[0], dxy2[1]} {
		case [4]int{1, 0, -1, 0}, [4]int{-1, 0, 1, 0}:
			loop[start] = '-'
		case [4]int{1, 0, 0, 1}, [4]int{0, 1, 1, 0}:
			loop[start] = 'F'
		case [4]int{1, 0, 0, -1}, [4]int{0, -1, 1, 0}:
			loop[start] = 'L'
		case [4]int{0, 1, 0, -1}, [4]int{0, -1, 0, 1}:
			loop[start] = '|'
		case [4]int{-1, 0, 0, 1}, [4]int{0, 1, -1, 0}:
			loop[start] = '7'
		case [4]int{-1, 0, 0, -1}, [4]int{0, -1, -1, 0}:
			loop[start] = 'J'
		default:
			panic("?")
		}
		for xy, dxy := aoc.step(start, dxy1, grid); xy != start; xy, dxy = aoc.step(xy, dxy, grid) {
			loop[xy] = grid[xy]
		}
	}

	in := make(chan int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for y := range in {
				inLoop := false
				for x := range w {
					switch loop[[2]int{x, y}] {
					case '-', 'F', '7':
					case '|', 'L', 'J':
						inLoop = !inLoop
					default:
						if inLoop {
							result++
						}
					}
				}
			}
			out <- result
		}()
	}

	for y := range h {
		in <- y
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
