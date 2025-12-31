package main

import (
	"fmt"
)

func init() {
	Register(&aoc202318{
		AOC: AOC{
			Day:           18,
			InputFilename: "../2023/input/18.txt",
			Tests: []Test{
				Test{
					Input: `R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
`,
					Part1: "62",
					Part2: "952408144115",
				},
				Test{
					Input: `D 2 (#7a21e1)
R 2 (#015230)
D 3 (#a77fa1)
L 2 (#7807d0)
D 2 (#caa173)
R 1 (#1b58a0)
D 2 (#caa171)
R 5 (#8ceee0)
U 2 (#411b93)
L 2 (#59c682)
U 2 (#d2c083)
R 2 (#5713f2)
U 5 (#0dc573)
L 6 (#70c712)
`,
					Part1: "62",
					Part2: "952408144115",
				},
			},
		},
	})
}

type aoc202318 struct {
	AOC
}

func (aoc *aoc202318) next(part1 bool, input *Input) (int, int, bool) {
	dir, _ := input.Word()
	meters, _ := input.Int()
	color, ok := input.Word()
	if !ok {
		return 0, 0, false
	}
	if part1 {
		switch dir {
		case "U":
			return DirU, meters, true
		case "D":
			return DirD, meters, true
		case "L":
			return DirL, meters, true
		case "R":
			return DirR, meters, true
		default:
			panic("bad input")
		}
	} else {
		fmt.Sscanf(color[2:7], "%x", &meters)
		switch color[7] {
		case '0':
			return DirR, meters, true
		case '1':
			return DirD, meters, true
		case '2':
			return DirL, meters, true
		case '3':
			return DirU, meters, true
		default:
			panic("bad input")
		}
	}
}

func (aoc *aoc202318) result(part1 bool, input *Input) string {
	// If the loop is clockwise, the trench would be on the right.
	// If the loop is counter clockwise, the trench would be on the left.
	firstDir, firstDist, _ := aoc.next(part1, input)
	xy := [2]int{}
	lastDir := firstDir
	area := 1
	border := 0
	for dir, dist, ok := aoc.next(part1, input); ok; dir, dist, ok = aoc.next(part1, input) {
		border += dist
		switch [2]int{lastDir, dir} {
		case [2]int{DirR, DirU}:
			area += 0
			xy = AdvanceXY(xy, DirU, dist)
		case [2]int{DirR, DirD}:
			area += dist
			xy = AdvanceXY(xy, DirD, dist)
		case [2]int{DirL, DirU}:
			area -= 0
			xy = AdvanceXY(xy, DirU, dist)
		case [2]int{DirL, DirD}:
			area += dist
			xy = AdvanceXY(xy, DirD, dist)
		case [2]int{DirU, DirL}:
			area += xy[1] * dist
			xy = AdvanceXY(xy, DirL, dist)
		case [2]int{DirU, DirR}:
			area -= (xy[1] - 1) * dist
			xy = AdvanceXY(xy, DirR, dist)
		case [2]int{DirD, DirL}:
			area += xy[1] * dist
			xy = AdvanceXY(xy, DirL, dist)
		case [2]int{DirD, DirR}:
			area -= (xy[1] - 1) * dist
			xy = AdvanceXY(xy, DirR, dist)
		default:
			panic("bad input")
		}
		lastDir = dir
	}
	dir := firstDir
	dist := firstDist
	border += dist
	switch [2]int{lastDir, dir} {
	case [2]int{DirR, DirU}:
		area += 0
		xy = AdvanceXY(xy, DirU, dist)
	case [2]int{DirR, DirD}:
		area += dist
		xy = AdvanceXY(xy, DirD, dist)
	case [2]int{DirL, DirU}:
		area -= 0
		xy = AdvanceXY(xy, DirU, dist)
	case [2]int{DirL, DirD}:
		area += dist
		xy = AdvanceXY(xy, DirD, dist)
	case [2]int{DirU, DirL}:
		area += xy[1] * dist
		xy = AdvanceXY(xy, DirL, dist)
	case [2]int{DirU, DirR}:
		area -= (xy[1] - 1) * dist
		xy = AdvanceXY(xy, DirR, dist)
	case [2]int{DirD, DirL}:
		area += xy[1] * dist
		xy = AdvanceXY(xy, DirL, dist)
	case [2]int{DirD, DirR}:
		area -= (xy[1] - 1) * dist
		xy = AdvanceXY(xy, DirR, dist)
	default:
		panic("bad input")
	}
	return IntResult(max(area, border-area+2)) //  either clockwise or counterclockwise
}

func (aoc *aoc202318) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202318) Part2(input *Input) string {
	return aoc.result(false, input)
}
