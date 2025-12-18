package main

import (
	"container/list"
)

func init() {
	Register(&aoc202412{
		AOC: AOC{
			Day:           12,
			InputFilename: "../2024/input/12.txt",
			Tests: []Test{
				Test{
					Input: `AAAA
BBCD
BBCC
EEEC
`,
					Part1: "140",
					Part2: "80",
				},
				Test{
					Input: `OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
`,
					Part1: "772",
					Part2: "436",
				},
				Test{
					Input: `RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
`,
					Part1: "1930",
					Part2: "1206",
				},
				Test{
					Input: `AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
`,
					Part1: "",
					Part2: "368",
				},
			},
		},
	})
}

type aoc202412 struct {
	AOC
}

func (aoc *aoc202412) markRegion(grid map[[2]int]byte, xy [2]int, marks map[[2]int]bool) map[[2]int][2]int {
	region := map[[2]int][2]int{}
	regionid := grid[xy]
	queue := list.New()
	queue.PushFront(xy)
	for {
		e := queue.Front()
		if e == nil {
			return region
		}
		queue.Remove(e)
		xy := e.Value.([2]int)
		if marks[xy] {
			continue
		}
		marks[xy] = true
		fences := 4
		leftFence := true
		if regionid == grid[[2]int{xy[0] - 1, xy[1]}] {
			fences--
			leftFence = false
			if !marks[[2]int{xy[0] - 1, xy[1]}] {
				queue.PushFront([2]int{xy[0] - 1, xy[1]})
			}
		}
		rightFence := true
		if regionid == grid[[2]int{xy[0] + 1, xy[1]}] {
			fences--
			rightFence = false
			if !marks[[2]int{xy[0] + 1, xy[1]}] {
				queue.PushFront([2]int{xy[0] + 1, xy[1]})
			}
		}
		upFence := true
		if regionid == grid[[2]int{xy[0], xy[1] - 1}] {
			fences--
			upFence = false
			if !marks[[2]int{xy[0], xy[1] - 1}] {
				queue.PushFront([2]int{xy[0], xy[1] - 1})
			}
		}
		downFence := true
		if regionid == grid[[2]int{xy[0], xy[1] + 1}] {
			fences--
			downFence = false
			if !marks[[2]int{xy[0], xy[1] + 1}] {
				queue.PushFront([2]int{xy[0], xy[1] + 1})
			}
		}
		fenceEnds := 0
		if leftFence {
			if upFence {
				fenceEnds++
			} else if regionid == grid[[2]int{xy[0] - 1, xy[1] - 1}] {
				fenceEnds++
			}
		}
		if rightFence {
			if upFence {
				fenceEnds++
			} else if regionid == grid[[2]int{xy[0] + 1, xy[1] - 1}] {
				fenceEnds++
			}
		}
		if upFence {
			if leftFence {
				fenceEnds++
			} else if regionid == grid[[2]int{xy[0] - 1, xy[1] - 1}] {
				fenceEnds++
			}
		}
		if downFence {
			if leftFence {
				fenceEnds++
			} else if regionid == grid[[2]int{xy[0] - 1, xy[1] + 1}] {
				fenceEnds++
			}
		}
		region[xy] = [2]int{fences, fenceEnds}
	}
}

func (aoc *aoc202412) parse(input *Input) []map[[2]int][2]int {
	_, _, grid := input.Grid()
	var regions []map[[2]int][2]int
	marks := map[[2]int]bool{}
	for xy := range grid {
		if marks[xy] {
			continue
		}
		regions = append(regions, aoc.markRegion(grid, xy, marks))
	}

	return regions
}

func (aoc *aoc202412) cost(region map[[2]int][2]int) int {
	area := len(region)
	perimeter := 0
	for _, plot := range region {
		perimeter += plot[0]
	}
	return area * perimeter
}

func (aoc *aoc202412) Part1(input *Input) string {
	result := 0
	for _, region := range aoc.parse(input) {
		result += aoc.cost(region)
	}
	return IntResult(result)
}

func (aoc *aoc202412) bulkCost(region map[[2]int][2]int) int {
	area := len(region)
	sides := 0
	for _, plot := range region {
		sides += plot[1]
	}
	return area * sides
}

func (aoc *aoc202412) Part2(input *Input) string {
	result := 0
	for _, region := range aoc.parse(input) {
		result += aoc.bulkCost(region)
	}
	return IntResult(result)
}
