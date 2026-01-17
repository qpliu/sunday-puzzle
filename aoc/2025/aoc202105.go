package main

func init() {
	Register(&aoc202105{
		AOC: AOC{
			Day:           5,
			InputFilename: "../2021/input/05.txt",
			Tests: []Test{
				Test{
					Input: `0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
`,
					Part1: "5",
					Part2: "12",
				},
			},
		},
	})
}

type aoc202105 struct {
	AOC
}

func (aoc *aoc202105) parse(input *Input) Seq[[3]XY] {
	return func(yield func([3]XY) bool) {
		for {
			x1, _ := input.Int()
			y1, _ := input.Int()
			x2, _ := input.Int()
			y2, ok := input.Int()
			if !ok {
				return
			}
			dx := x2 - x1
			if dx > 0 {
				dx = 1
			} else if dx < 0 {
				dx = -1
			}
			dy := y2 - y1
			if dy > 0 {
				dy = 1
			} else if dy < 0 {
				dy = -1
			}
			if !yield([3]XY{XY{x1, y1}, XY{x2, y2}, XY{dx, dy}}) {
				return
			}
		}
	}
}

func (aoc *aoc202105) Part1(input *Input) string {
	grid := map[XY]int{}
	for vent := range aoc.parse(input) {
		dxy := vent[2]
		if dxy[0] != 0 && dxy[1] != 0 {
			continue
		}
		grid[vent[0]]++
		for xy := vent[0]; xy != vent[1]; {
			xy[0] += dxy[0]
			xy[1] += dxy[1]
			grid[xy]++
		}
	}
	result := 0
	for _, n := range grid {
		if n > 1 {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202105) Part2(input *Input) string {
	grid := map[XY]int{}
	for vent := range aoc.parse(input) {
		dxy := vent[2]
		grid[vent[0]]++
		for xy := vent[0]; xy != vent[1]; {
			xy[0] += dxy[0]
			xy[1] += dxy[1]
			grid[xy]++
		}
	}
	result := 0
	for _, n := range grid {
		if n > 1 {
			result++
		}
	}
	return IntResult(result)
}
