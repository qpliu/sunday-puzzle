package main

import (
	"fmt"
)

func init() {
	Register(&aoc202418{
		AOC: AOC{
			Day:           18,
			InputFilename: "../2024/input/18.txt",
			Tests: []Test{
				Test{
					Input: `5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
`,
					Part1: "22",
					Part2: "6,1",
				},
			},
		},
	})
}

type aoc202418 struct {
	AOC
}

func (aoc *aoc202418) next(input *Input) XY {
	x, _ := input.Int()
	y, _ := input.Int()
	return XY{x, y}
}

func (aoc *aoc202418) part1(size, corrupt int, input *Input) string {
	grid := map[XY]bool{}
	for range corrupt {
		grid[aoc.next(input)] = true
	}
	for i := range size + 1 {
		grid[XY{-1, i}] = true
		grid[XY{size + 1, i}] = true
		grid[XY{i, -1}] = true
		grid[XY{i, size + 1}] = true
	}

	type path struct {
		xy    XY
		steps int
	}

	done := func(p path) bool {
		return p.xy[0] == size && p.xy[1] == size
	}

	priority := func(p path) int {
		dx := size - p.xy[0]
		dy := size - p.xy[1]
		return -p.steps - dx - dy
	}

	state := func(p path) XY {
		return p.xy
	}

	neighbors := func(p path) []path {
		n := []path{}
		for _, nextXY := range [...]XY{
			AdvanceXY(p.xy, DirR, 1),
			AdvanceXY(p.xy, DirD, 1),
			AdvanceXY(p.xy, DirL, 1),
			AdvanceXY(p.xy, DirU, 1),
		} {
			if !grid[nextXY] {
				n = append(n, path{nextXY, p.steps + 1})
			}
		}
		return n
	}

	p, ok := AstarSearch([]path{path{}}, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.steps)
}

func (aoc *aoc202418) Test1(input *Input) string {
	return aoc.part1(6, 12, input)
}

func (aoc *aoc202418) Part1(input *Input) string {
	return aoc.part1(70, 1024, input)
}

func (aoc *aoc202418) part2(size int, input *Input) string {
	grid := map[XY]int{}
	queue := NewQueue[XY]()
	for {
		xy := aoc.next(input)
		blocks := 1 // connected to neither lower left or upper right
		// assume (0,0) and (size,size) never get corrupted
		if xy[0] == 0 || xy[1] == size {
			blocks = 2 // connected to lower left
		} else if xy[0] == size || xy[1] == 0 {
			blocks = 3 // connected to upper right
		}
		queue.Enqueue(xy)
		for !queue.Empty() {
			qxy := queue.Dequeue()
			if grid[qxy] == blocks {
				continue
			}
			grid[qxy] = blocks
			for _, nxy := range [...]XY{
				XY{qxy[0] + 1, qxy[1]},
				XY{qxy[0] + 1, qxy[1] + 1},
				XY{qxy[0], qxy[1] + 1},
				XY{qxy[0] - 1, qxy[1] + 1},
				XY{qxy[0] - 1, qxy[1]},
				XY{qxy[0] - 1, qxy[1] - 1},
				XY{qxy[0], qxy[1] - 1},
				XY{qxy[0] + 1, qxy[1] - 1},
			} {
				if nxy[0] < 0 || nxy[0] > size || nxy[1] < 0 || nxy[1] > size {
					continue
				}
				blocks2 := grid[nxy]
				if blocks2 == 0 || blocks2 == blocks {
					continue
				}
				if blocks+blocks2 == 5 {
					return fmt.Sprintf("%d,%d", xy[0], xy[1])
				}
				if blocks > blocks2 {
					queue.Enqueue(nxy)
				} else {
					blocks = blocks2
					queue.Enqueue(qxy)
				}
			}
		}
	}
}

func (aoc *aoc202418) Test2(input *Input) string {
	return aoc.part2(6, input)
}

func (aoc *aoc202418) Part2(input *Input) string {
	return aoc.part2(70, input)
}
