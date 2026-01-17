package main

func init() {
	Register(&aoc202115{
		AOC: AOC{
			Day:           15,
			InputFilename: "../2021/input/15.txt",
			Tests: []Test{
				Test{
					Input: `1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581
`,
					Part1: "40",
					Part2: "315",
				},
			},
		},
	})
}

type aoc202115 struct {
	AOC
}

func (aoc *aoc202115) result(repeats int, input *Input) string {
	width, height, grid := input.Grid()
	goal := XY{width*repeats - 1, height*repeats - 1}

	type path struct {
		xy   XY
		risk int
	}

	start := []path{path{XY{}, 0}}
	done := func(p path) bool {
		return p.xy == goal
	}
	priority := func(p path) int {
		// 0 does not appear in the example, nor does it appear in my input
		return -(p.risk + goal[0] - p.xy[0] + goal[1] - p.xy[1])
	}
	state := func(p path) XY {
		return p.xy
	}
	neighbors := func(p path) []path {
		n := []path{}
		for _, xy := range [...]XY{
			AdvanceXY(p.xy, DirR, 1),
			AdvanceXY(p.xy, DirD, 1),
			AdvanceXY(p.xy, DirL, 1),
			AdvanceXY(p.xy, DirU, 1),
		} {
			if xy[0] < 0 || xy[1] < 0 || xy[0] >= width*repeats || xy[1] >= height*repeats {
				continue
			}
			risk := int(grid[XY{xy[0] % width, xy[1] % height}]-'0') + xy[0]/width + xy[1]/height
			if risk > 9 {
				risk -= 9
			}
			n = append(n, path{xy, p.risk + risk})
		}
		return n
	}

	result, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(result.risk)
}

func (aoc *aoc202115) Part1(input *Input) string {
	return aoc.result(1, input)
}

func (aoc *aoc202115) Part2(input *Input) string {
	return aoc.result(5, input)
}
