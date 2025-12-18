package main

import (
	"container/list"
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

func (aoc *aoc202418) next(input *Input) [2]int {
	x, _ := input.Int()
	y, _ := input.Int()
	return [2]int{x, y}
}

type aoc202418_state struct {
	grid     map[[2]int]bool
	xy, goal [2]int
	steps    int
}

func (p aoc202418_state) Done() bool {
	return p.xy == p.goal
}

func (p aoc202418_state) Priority() int {
	dx := p.xy[0] - p.goal[0]
	dy := p.xy[1] - p.goal[1]
	return -p.steps - max(dx, -dx) - max(dy, -dy)
}

func (p aoc202418_state) State() [2]int {
	return p.xy
}

func (p aoc202418_state) Neighbors() []AstarPath[[2]int] {
	n := []AstarPath[[2]int]{}
	for _, nextXY := range [4][2]int{
		[2]int{p.xy[0] + 1, p.xy[1]},
		[2]int{p.xy[0], p.xy[1] + 1},
		[2]int{p.xy[0] - 1, p.xy[1]},
		[2]int{p.xy[0], p.xy[1] - 1},
	} {
		if p.grid[nextXY] {
			continue
		}
		if nextXY[0] < 0 || nextXY[1] < 0 || nextXY[0] > p.goal[0] || nextXY[1] > p.goal[1] {
			continue
		}
		n = append(n, aoc202418_state{
			grid:  p.grid,
			xy:    nextXY,
			goal:  p.goal,
			steps: p.steps + 1,
		})
	}
	return n
}

func (aoc *aoc202418) part1(size, corrupt int, input *Input) string {
	grid := map[[2]int]bool{}
	for _ = range corrupt {
		grid[aoc.next(input)] = true
	}
	path := AstarSearch([]AstarPath[[2]int]{
		aoc202418_state{
			grid:  grid,
			xy:    [2]int{0, 0},
			goal:  [2]int{size, size},
			steps: 0,
		},
	})
	return IntResult(path.(aoc202418_state).steps)
}

func (aoc *aoc202418) Test1(input *Input) string {
	return aoc.part1(6, 12, input)
}

func (aoc *aoc202418) Part1(input *Input) string {
	return aoc.part1(70, 1024, input)
}

func (aoc *aoc202418) part2(size int, input *Input) string {
	grid := map[[2]int]int{}
	queue := list.New()
	for {
		xy := aoc.next(input)
		blocks := 1
		// assume (0,0) and (size,size) never get corrupted
		if xy[0] == 0 || xy[1] == size {
			blocks = 2
		} else if xy[0] == size || xy[1] == 0 {
			blocks = 3
		}
		queue.PushFront(xy)
		for queue.Len() > 0 {
			e := queue.Front()
			queue.Remove(e)
			qxy := e.Value.([2]int)
			if grid[qxy] == blocks {
				continue
			}
			grid[qxy] = blocks
			for _, nxy := range [8][2]int{
				[2]int{qxy[0] + 1, qxy[1]},
				[2]int{qxy[0] + 1, qxy[1] + 1},
				[2]int{qxy[0], qxy[1] + 1},
				[2]int{qxy[0] - 1, qxy[1] + 1},
				[2]int{qxy[0] - 1, qxy[1]},
				[2]int{qxy[0] - 1, qxy[1] - 1},
				[2]int{qxy[0], qxy[1] - 1},
				[2]int{qxy[0] + 1, qxy[1] - 1},
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
					queue.PushFront(nxy)
				} else {
					blocks = blocks2
					queue.PushFront(qxy)
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
