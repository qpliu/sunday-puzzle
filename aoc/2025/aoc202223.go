package main

import (
	"math"
)

func init() {
	Register(&aoc202223{
		AOC: AOC{
			Day:           23,
			InputFilename: "../2022/input/23.txt",
			Tests: []Test{
				Test{
					Input: `....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
`,
					Part1: "110",
					Part2: "20",
				},
			},
		},
	})
}

type aoc202223 struct {
	AOC

	dirs [4][4][3]int
}

func (aoc *aoc202223) initDirs() {
	aoc.dirs = [4][4][3]int{
		[4][3]int{
			[3]int{DirU, DirU | DirL, DirU | DirR},
			[3]int{DirD, DirD | DirR, DirD | DirL},
			[3]int{DirL, DirL | DirD, DirL | DirU},
			[3]int{DirR, DirR | DirU, DirR | DirD},
		},
		[4][3]int{
			[3]int{DirD, DirD | DirR, DirD | DirL},
			[3]int{DirL, DirL | DirD, DirL | DirU},
			[3]int{DirR, DirR | DirU, DirR | DirD},
			[3]int{DirU, DirU | DirL, DirU | DirR},
		},
		[4][3]int{
			[3]int{DirL, DirL | DirD, DirL | DirU},
			[3]int{DirR, DirR | DirU, DirR | DirD},
			[3]int{DirU, DirU | DirL, DirU | DirR},
			[3]int{DirD, DirD | DirR, DirD | DirL},
		},
		[4][3]int{
			[3]int{DirR, DirR | DirU, DirR | DirD},
			[3]int{DirU, DirU | DirL, DirU | DirR},
			[3]int{DirD, DirD | DirR, DirD | DirL},
			[3]int{DirL, DirL | DirD, DirL | DirU},
		},
	}
}

func (aoc *aoc202223) parse(input *Input) map[XY]bool {
	x := 0
	y := 0
	elves := map[XY]bool{}
	for ch := range input.Chars() {
		switch ch {
		case '\n':
			y++
			x = -1
		case '#':
			elves[XY{x, y}] = true
		}
		x++
	}
	return elves
}

func (aoc *aoc202223) round(t int, elves map[XY]bool) int {
	propCounts := map[XY]int{}
	props := [][2]XY{}
	dirs := aoc.dirs[t%len(aoc.dirs)]

	for xy := range elves {
		d0 := elves[AdvanceXY(xy, dirs[0][0], 1)]
		d0a := elves[AdvanceXY(xy, dirs[0][1], 1)]
		d1 := elves[AdvanceXY(xy, dirs[1][0], 1)]
		d1a := elves[AdvanceXY(xy, dirs[1][1], 1)]
		d2 := elves[AdvanceXY(xy, dirs[2][0], 1)]
		d2a := elves[AdvanceXY(xy, dirs[2][1], 1)]
		d3 := elves[AdvanceXY(xy, dirs[3][0], 1)]
		d3a := elves[AdvanceXY(xy, dirs[3][1], 1)]
		if !(d0 || d0a || d1 || d1a || d2 || d2a || d3 || d3a) {
			continue
		}
		var pxy XY
		if !(d0 || d0a || elves[AdvanceXY(xy, dirs[0][2], 1)]) {
			pxy = AdvanceXY(xy, dirs[0][0], 1)
		} else if !(d1 || d1a || elves[AdvanceXY(xy, dirs[1][2], 1)]) {
			pxy = AdvanceXY(xy, dirs[1][0], 1)
		} else if !(d2 || d2a || elves[AdvanceXY(xy, dirs[2][2], 1)]) {
			pxy = AdvanceXY(xy, dirs[2][0], 1)
		} else if !(d3 || d3a || elves[AdvanceXY(xy, dirs[3][2], 1)]) {
			pxy = AdvanceXY(xy, dirs[3][0], 1)
		} else {
			continue
		}
		c := propCounts[pxy]
		if c == 0 {
			props = append(props, [2]XY{xy, pxy})
			propCounts[pxy] = 1
		} else if c == 1 {
			propCounts[pxy] = 2
		}
	}

	moveCount := 0
	for _, prop := range props {
		if propCounts[prop[1]] == 1 {
			moveCount++
			delete(elves, prop[0])
			elves[prop[1]] = true
		}
	}
	return moveCount
}

func (aoc *aoc202223) Part1(input *Input) string {
	elves := aoc.parse(input)
	aoc.initDirs()
	for t := range 10 {
		aoc.round(t, elves)
	}

	xmin, xmax := math.MaxInt, math.MinInt
	ymin, ymax := math.MaxInt, math.MinInt
	for xy := range elves {
		xmin = min(xmin, xy[0])
		xmax = max(xmax, xy[0])
		ymin = min(ymin, xy[1])
		ymax = max(ymax, xy[1])
	}
	return IntResult((xmax-xmin+1)*(ymax-ymin+1) - len(elves))
}

func (aoc *aoc202223) Part2(input *Input) string {
	elves := aoc.parse(input)
	aoc.initDirs()

	t := 0
	for {
		if aoc.round(t, elves) == 0 {
			return IntResult(t + 1)
		}
		t++
	}
}
