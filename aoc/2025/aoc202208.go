package main

import (
	"runtime"
)

func init() {
	Register(&aoc202208{
		AOC: AOC{
			Day:           8,
			InputFilename: "../2022/input/08.txt",
			Tests: []Test{
				Test{
					Input: `30373
25512
65332
33549
35390
`,
					Part1: "21",
					Part2: "8",
				},
			},
		},
	})
}

type aoc202208 struct {
	AOC
}

func (aoc *aoc202208) Part1(input *Input) string {
	w, h, grid := input.Grid()
	visible := map[XY]bool{}

	for x := range w {
		t := grid[XY{x, 0}]
		visible[XY{x, 0}] = true
		for y := 1; y < h; y++ {
			tt := grid[XY{x, y}]
			if tt > t {
				visible[XY{x, y}] = true
				t = tt
			}
		}
		t = grid[XY{x, h - 1}]
		visible[XY{x, h - 1}] = true
		for y := h - 2; y > 0; y-- {
			tt := grid[XY{x, y}]
			if tt > t {
				visible[XY{x, y}] = true
				t = tt
			}
		}
	}
	for y := range h {
		t := grid[XY{0, y}]
		visible[XY{0, y}] = true
		for x := 1; x < w; x++ {
			tt := grid[XY{x, y}]
			if tt > t {
				visible[XY{x, y}] = true
				t = tt
			}
		}
		t = grid[XY{w - 1, y}]
		visible[XY{w - 1, y}] = true
		for x := w - 2; x > 0; x-- {
			tt := grid[XY{x, y}]
			if tt > t {
				visible[XY{x, y}] = true
				t = tt
			}
		}
	}
	return IntResult(len(visible))
}

func (aoc *aoc202208) Part2(input *Input) string {
	w, h, grid := input.Grid()

	in := make(chan XY)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for xy := range in {
				x, y, t := xy[0], xy[1], grid[xy]
				score := 1
				n := 1
				for xx := x + 1; xx < w-1 && grid[XY{xx, y}] < t; xx++ {
					n++
				}
				score *= n
				n = 1
				for xx := x - 1; xx > 0 && grid[XY{xx, y}] < t; xx-- {
					n++
				}
				score *= n
				n = 1
				for yy := y + 1; yy < h-1 && grid[XY{x, yy}] < t; yy++ {
					n++
				}
				score *= n
				n = 1
				for yy := y - 1; yy > 0 && grid[XY{x, yy}] < t; yy-- {
					n++
				}
				score *= n
				result = max(result, score)
			}
			out <- result
		}()
	}
	for xy := range grid {
		if xy[0] > 0 && xy[1] > 0 && xy[0] < w-1 && xy[1] < h-1 {
			in <- xy
		}
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result = max(result, <-out)
	}
	return IntResult(result)
}
