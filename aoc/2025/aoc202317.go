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

type aoc202317_state struct {
	grid     map[XY]byte
	goal     XY
	minMoves int
	maxMoves int
	xyDir    XYDir
	loss     int
}

func (s aoc202317_state) Done() bool {
	return ToXY(s.xyDir) == s.goal
}

func (s aoc202317_state) Priority() int {
	dx := s.xyDir[0] - s.goal[0]
	dy := s.xyDir[1] - s.goal[1]
	// there are no blocks with 0 loss
	return -s.loss - max(dx, -dx) - max(dy, -dy)
}

func (s aoc202317_state) State() XYDir {
	return s.xyDir
}

func (s aoc202317_state) Neighbors() []AstarPath[XYDir] {
	n := []AstarPath[XYDir]{}
	xyDir := s.xyDir
	loss := s.loss
	for i := range s.maxMoves {
		xyDir = AdvanceXYDir(xyDir, 1)
		block, ok := s.grid[ToXY(xyDir)]
		if !ok {
			break
		}
		loss += int(block - '0')
		if i+1 >= s.minMoves {
			n = append(n, aoc202317_state{
				grid:     s.grid,
				goal:     s.goal,
				minMoves: s.minMoves,
				maxMoves: s.maxMoves,
				xyDir:    XYDirTurnL(xyDir),
				loss:     loss,
			}, aoc202317_state{
				grid:     s.grid,
				goal:     s.goal,
				minMoves: s.minMoves,
				maxMoves: s.maxMoves,
				xyDir:    XYDirTurnR(xyDir),
				loss:     loss,
			})
		}
	}
	return n
}

func (aoc *aoc202317) Part1(input *Input) string {
	w, h, grid := input.Grid()
	s := AstarSearch([]AstarPath[XYDir]{
		aoc202317_state{
			grid:     grid,
			goal:     XY{w - 1, h - 1},
			minMoves: 1,
			maxMoves: 3,
			xyDir:    XYDir{0, 0, DirR},
			loss:     0,
		},
		aoc202317_state{
			grid:     grid,
			goal:     XY{w - 1, h - 1},
			minMoves: 1,
			maxMoves: 3,
			xyDir:    XYDir{0, 0, DirD},
			loss:     0,
		},
	})
	return IntResult(s.(aoc202317_state).loss)
}

func (aoc *aoc202317) Part2(input *Input) string {
	w, h, grid := input.Grid()
	s := AstarSearch([]AstarPath[XYDir]{
		aoc202317_state{
			grid:     grid,
			goal:     XY{w - 1, h - 1},
			minMoves: 4,
			maxMoves: 10,
			xyDir:    XYDir{0, 0, DirR},
			loss:     0,
		},
		aoc202317_state{
			grid:     grid,
			goal:     XY{w - 1, h - 1},
			minMoves: 4,
			maxMoves: 10,
			xyDir:    XYDir{0, 0, DirD},
			loss:     0,
		},
	})
	return IntResult(s.(aoc202317_state).loss)
}
