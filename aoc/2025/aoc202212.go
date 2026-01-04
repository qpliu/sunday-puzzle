package main

func init() {
	Register(&aoc202212{
		AOC: AOC{
			Day:           12,
			InputFilename: "../2022/input/12.txt",
			Tests: []Test{
				Test{
					Input: `Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
`,
					Part1: "31",
					Part2: "29",
				},
			},
		},
	})
}

type aoc202212 struct {
	AOC
}

func (aoc *aoc202212) result(part1 bool, input *Input) string {
	_, _, grid := input.Grid()

	type path struct {
		xy    XY
		steps int
	}

	var goal XY
	start := []path{}
	for xy, ch := range grid {
		switch ch {
		case 'S':
			grid[xy] = 'a'
			start = append(start, path{xy: xy})
		case 'a':
			if !part1 {
				start = append(start, path{xy: xy})
			}
		case 'E':
			grid[xy] = 'z'
			goal = xy
		}
	}

	done := func(p path) bool {
		return p.xy == goal
	}

	priority := func(p path) int {
		dx := p.xy[0] - goal[0]
		dy := p.xy[1] - goal[1]
		return -p.steps - max(dx, -dx) - max(dy, -dy)
	}

	state := func(p path) XY {
		return p.xy
	}

	neighbors := func(p path) []path {
		n := []path{}
		h := grid[p.xy]
		for _, xy := range [...]XY{
			AdvanceXY(p.xy, DirR, 1),
			AdvanceXY(p.xy, DirD, 1),
			AdvanceXY(p.xy, DirL, 1),
			AdvanceXY(p.xy, DirU, 1),
		} {
			hdest := grid[xy]
			if hdest != 0 && hdest <= h+1 {
				n = append(n, path{xy: xy, steps: p.steps + 1})
			}
		}
		return n
	}

	p, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.steps)
}

func (aoc *aoc202212) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202212) Part2(input *Input) string {
	return aoc.result(false, input)
}
