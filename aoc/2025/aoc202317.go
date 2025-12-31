package main

func init() {
	Register(&aoc202317{
		AOC: AOC{
			Day:           17,
			InputFilename: "../2023/input/17.txt",
			Tests: []Test{
				Test{
					Input: `2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
`,
					Part1: "102",
					Part2: "94",
				},
				Test{
					Input: `111111111111
999999999991
999999999991
999999999991
999999999991
`,
					Part1: "",
					Part2: "71",
				},
			},
		},
	})
}

type aoc202317 struct {
	AOC
}

func (aoc *aoc202317) result(part1 bool, input *Input) string {
	type path struct {
		xyDir XYDir
		loss  int
	}

	w, h, grid := input.Grid()

	goal := XY{w - 1, h - 1}
	done := func(p path) bool {
		return ToXY(p.xyDir) == goal
	}

	priority := func(p path) int {
		dx := p.xyDir[0] - goal[0]
		dy := p.xyDir[1] - goal[1]
		// there are no blocks with 0 loss
		return -p.loss - max(dx, -dx) - max(dy, -dy)
	}

	state := func(p path) XYDir {
		return p.xyDir
	}

	var minMoves, maxMoves int
	neighbors := func(p path) []path {
		n := []path{}
		xyDir := p.xyDir
		loss := p.loss
		for i := range maxMoves {
			xyDir = AdvanceXYDir(xyDir, 1)
			block, ok := grid[ToXY(xyDir)]
			if !ok {
				break
			}
			loss += int(block - '0')
			if i+1 >= minMoves {
				n = append(n, path{
					xyDir: XYDirTurnL(xyDir),
					loss:  loss,
				}, path{
					xyDir: XYDirTurnR(xyDir),
					loss:  loss,
				})
			}
		}
		return n
	}

	start := []path{path{XYDir{0, 0, DirR}, 0}, path{XYDir{0, 0, DirD}, 0}}

	if part1 {
		minMoves, maxMoves = 1, 3
	} else {
		minMoves, maxMoves = 4, 10
	}
	p, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.loss)
}

func (aoc *aoc202317) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202317) Part2(input *Input) string {
	return aoc.result(false, input)
}
