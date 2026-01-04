package main

import (
	"math"
)

func init() {
	Register(&aoc202218{
		AOC: AOC{
			Day:           18,
			InputFilename: "../2022/input/18.txt",
			Tests: []Test{
				Test{
					Input: `2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
`,
					Part1: "64",
					Part2: "58",
				},
			},
		},
	})
}

type aoc202218 struct {
	AOC
}

func (aoc *aoc202218) parse(input *Input) ([3][2]int, map[[3]int]bool) {
	xyzRanges := [3][2]int{
		[2]int{math.MaxInt, math.MinInt},
		[2]int{math.MaxInt, math.MinInt},
		[2]int{math.MaxInt, math.MinInt},
	}
	droplet := map[[3]int]bool{}
	for {
		x, _ := input.Int()
		y, _ := input.Int()
		z, ok := input.Int()
		if !ok {
			return xyzRanges, droplet
		}
		droplet[[3]int{x, y, z}] = true
		xyzRanges[0][0] = min(x, xyzRanges[0][0])
		xyzRanges[0][1] = max(x, xyzRanges[0][1])
		xyzRanges[1][0] = min(y, xyzRanges[1][0])
		xyzRanges[1][1] = max(y, xyzRanges[1][1])
		xyzRanges[2][0] = min(z, xyzRanges[2][0])
		xyzRanges[2][1] = max(z, xyzRanges[2][1])
	}
}

func (aoc *aoc202218) exposedFaces(droplet map[[3]int]bool) int {
	count := 0
	for xyz := range droplet {
		if !droplet[[3]int{xyz[0] + 1, xyz[1], xyz[2]}] {
			count++
		}
		if !droplet[[3]int{xyz[0] - 1, xyz[1], xyz[2]}] {
			count++
		}
		if !droplet[[3]int{xyz[0], xyz[1] + 1, xyz[2]}] {
			count++
		}
		if !droplet[[3]int{xyz[0], xyz[1] - 1, xyz[2]}] {
			count++
		}
		if !droplet[[3]int{xyz[0], xyz[1], xyz[2] + 1}] {
			count++
		}
		if !droplet[[3]int{xyz[0], xyz[1], xyz[2] - 1}] {
			count++
		}
	}
	return count
}

func (aoc *aoc202218) Part1(input *Input) string {
	_, droplet := aoc.parse(input)
	return IntResult(aoc.exposedFaces(droplet))
}

func (aoc *aoc202218) Part2(input *Input) string {
	xyzRange, droplet := aoc.parse(input)
	x1, x2 := xyzRange[0][0]-1, xyzRange[0][1]+1
	y1, y2 := xyzRange[1][0]-1, xyzRange[1][1]+1
	z1, z2 := xyzRange[2][0]-1, xyzRange[2][1]+1
	queue := NewQueue[[3]int]()
	mold := map[[3]int]bool{}
	queue.Enqueue([3]int{x1, y1, z1})
	for !queue.Empty() {
		xyz := queue.Dequeue()
		if mold[xyz] {
			continue
		}
		mold[xyz] = true
		if !droplet[[3]int{xyz[0] + 1, xyz[1], xyz[2]}] {
			if xyz[0]+1 <= x2 {
				queue.Enqueue([3]int{xyz[0] + 1, xyz[1], xyz[2]})
			}
		}
		if !droplet[[3]int{xyz[0] - 1, xyz[1], xyz[2]}] {
			if xyz[0]-1 >= x1 {
				queue.Enqueue([3]int{xyz[0] - 1, xyz[1], xyz[2]})
			}
		}
		if !droplet[[3]int{xyz[0], xyz[1] + 1, xyz[2]}] {
			if xyz[1]+1 <= y2 {
				queue.Enqueue([3]int{xyz[0], xyz[1] + 1, xyz[2]})
			}
		}
		if !droplet[[3]int{xyz[0], xyz[1] - 1, xyz[2]}] {
			if xyz[1]-1 >= y1 {
				queue.Enqueue([3]int{xyz[0], xyz[1] - 1, xyz[2]})
			}
		}
		if !droplet[[3]int{xyz[0], xyz[1], xyz[2] + 1}] {
			if xyz[2]+1 <= z2 {
				queue.Enqueue([3]int{xyz[0], xyz[1], xyz[2] + 1})
			}
		}
		if !droplet[[3]int{xyz[0], xyz[1], xyz[2] - 1}] {
			if xyz[2]-1 >= z1 {
				queue.Enqueue([3]int{xyz[0], xyz[1], xyz[2] - 1})
			}
		}
	}
	return IntResult(aoc.exposedFaces(mold) - 2*(x2-x1+1)*(y2-y1+1) - 2*(x2-x1+1)*(z2-z1+1) - 2*(y2-y1+1)*(z2-z1+1))
}
