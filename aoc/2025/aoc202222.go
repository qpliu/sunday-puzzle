package main

func init() {
	Register(&aoc202222{
		AOC: AOC{
			Day:           22,
			InputFilename: "../2022/input/22.txt",
			Tests: []Test{
				Test{
					Input: `        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5
`,
					Part1: "6032",
					Part2: "5031",
				},
			},
		},
	})
}

type aoc202222 struct {
	AOC
}

func (aoc *aoc202222) parse(input *Input) (map[XY]byte, Seq2[int, func(XYDir) XYDir]) {
	paragraph, _ := input.Paragraph()
	_, _, grid := InputString(paragraph).Grid()
	noTurn := func(xydir XYDir) XYDir {
		return xydir
	}

	return grid, func(yield func(int, func(XYDir) XYDir) bool) {
		for {
			count, ok := input.Int()
			if !ok {
				return
			}
			ch, _ := input.Char()
			turn := noTurn
			switch ch {
			case 'L':
				turn = XYDirTurnL
			case 'R':
				turn = XYDirTurnR
			}
			if !yield(count, turn) {
				return
			}
		}
	}
}

func (aoc *aoc202222) result(xydir XYDir) string {
	result := 4*(1+xydir[0]) + 1000*(1+xydir[1])
	switch xydir[2] {
	case DirR:
	case DirD:
		result += 1
	case DirL:
		result += 2
	case DirU:
		result += 3
	default:
		panic("?")
	}
	return IntResult(result)
}

func (aoc *aoc202222) Part1(input *Input) string {
	grid, moves := aoc.parse(input)
	xydir := XYDir{0, 0, DirR}
	for grid[ToXY(xydir)] != '.' {
		xydir = AdvanceXYDir(xydir, 1)
	}
	for count, turn := range moves {
	advanceLoop:
		for range count {
			nxydir := AdvanceXYDir(xydir, 1)
			switch grid[ToXY(nxydir)] {
			case '#':
				break advanceLoop
			case '.':
				xydir = nxydir
			default:
				nxydir := XYDirTurnL(XYDirTurnL(xydir))
				wasWall := false
				for {
					rxydir := AdvanceXYDir(nxydir, 1)
					switch grid[ToXY(rxydir)] {
					case '#':
						nxydir = rxydir
						wasWall = true
					case '.':
						nxydir = rxydir
						wasWall = false
					default:
						if wasWall {
							break advanceLoop
						}
						xydir = XYDirTurnL(XYDirTurnL(nxydir))
						continue advanceLoop
					}
				}
			}
		}
		xydir = turn(xydir)
	}
	return aoc.result(xydir)
}

func (aoc *aoc202222) foldCube(size int, grid map[XY]byte) map[XYDir]XYDir {
	onMap := func(x, y int) bool {
		switch grid[XY{x, y}] {
		case '#', '.':
			return true
		default:
			return false
		}
	}

	x := 0
	for !onMap(x, 0) {
		x++
	}

	// Shape of sample input:
	//   B
	// bLF
	//   TR

	// Shape of my input:
	//    BR
	//    F
	//   LT
	//   b

	folds := map[XYDir]XYDir{}

	// connect B to b, sample input
	if onMap(x, size) && onMap(x-size, size) && onMap(x-2*size, size) {
		for i := range size {
			folds[XYDir{x + i, -1, DirU}] = XYDir{x - size - 1 - i, size, DirD}
			folds[XYDir{x - size - 1 - i, size - 1, DirU}] = XYDir{x + i, 0, DirD}
		}
	}

	// connect B to b, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x-size, 2*size) && onMap(x-size, 3*size) {
		for i := range size {
			folds[XYDir{x + i, -1, DirU}] = XYDir{x - size, 3*size + i, DirR}
			folds[XYDir{x - size - 1, 3*size + i, DirL}] = XYDir{x + i, 0, DirD}
		}
	}

	// connect B to L, sample input
	if onMap(x, size) && onMap(x-size, size) {
		for i := range size {
			folds[XYDir{x - 1, i, DirL}] = XYDir{x - size + i, size, DirD}
			folds[XYDir{x - size + i, size - 1, DirU}] = XYDir{x, i, DirR}
		}
	}

	// connect B to L, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x-size, 2*size) {
		for i := range size {
			folds[XYDir{x - 1, i, DirL}] = XYDir{x - size, 3*size - 1 - i, DirR}
			folds[XYDir{x - size - 1, 3*size - 1 - i, DirL}] = XYDir{x, i, DirR}
		}
	}

	// connect B to F, sample input
	// connect B to F, my input

	// connect B to R, sample input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x+size, 2*size) {
		for i := range size {
			folds[XYDir{x + size, i, DirR}] = XYDir{x + 2*size - 1, 3*size - 1 - i, DirL}
			folds[XYDir{x + 2*size, 3*size - 1 - i, DirR}] = XYDir{x + size - 1, i, DirL}
		}
	}

	// connect B to R, my input

	// connect T to b, sample input
	if onMap(x, size) && onMap(x-size, size) && onMap(x-2*size, size) && onMap(x, 2*size) {
		for i := range size {
			folds[XYDir{x + i, 3 * size, DirD}] = XYDir{x - size - 1 - i, 2*size - 1, DirU}
			folds[XYDir{x - size - 1 - i, 2 * size, DirD}] = XYDir{x + i, 3*size - 1, DirU}
		}
	}

	// connect T to b, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x-size, 2*size) && onMap(x-size, 3*size) {
		for i := range size {
			folds[XYDir{x + i, 3 * size, DirD}] = XYDir{x - 1, 3*size + i, DirL}
			folds[XYDir{x, 3*size + i, DirR}] = XYDir{x + i, 3*size - 1, DirU}
		}
	}

	// connect T to L, sample input
	if onMap(x, size) && onMap(x, size) && onMap(x, 2*size) && onMap(x-size, size) {
		for i := range size {
			folds[XYDir{x - 1, 2*size + i, DirL}] = XYDir{x - 1 - i, 2*size - 1, DirU}
			folds[XYDir{x - 1 - i, 2 * size, DirD}] = XYDir{x, 2*size + i, DirR}
		}
	}

	// connect T to L, my input
	// connect T to F, sample input
	// connect T to F, my input
	// connect T to R, sample input

	// connect T to R, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(size, 0) {
		for i := range size {
			folds[XYDir{x + size, 2*size + i, DirR}] = XYDir{x + 2*size - 1, size - 1 - i, DirL}
			folds[XYDir{x + 2*size, size - 1 - i, DirR}] = XYDir{x + size - 1, 2*size + i, DirL}
		}
	}

	// connect L to b, sample input
	// connect L to b, my input
	// connect L to F, sample input

	// connect L to F, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x-size, 2*size) {
		for i := range size {
			folds[XYDir{x - size + i, 2*size - 1, DirU}] = XYDir{x, size + i, DirR}
			folds[XYDir{x - 1, size + i, DirL}] = XYDir{x - size + i, 2 * size, DirD}
		}
	}

	// connect R to b, sample input
	if onMap(x, size) && onMap(x-size, size) && onMap(x-2*size, size) && onMap(x, 2*size) && onMap(x+size, 2*size) {
		for i := range size {
			folds[XYDir{x + size + i, 3 * size, DirD}] = XYDir{x - 2*size, 2*size - 1 - i, DirR}
			folds[XYDir{x - 2*size - 1, 2*size - 1 - i, DirL}] = XYDir{x + size + i, 3*size - 1, DirU}
		}
	}

	// connect R to b, my input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x-size, 2*size) && onMap(x-size, 3*size) && onMap(x+size, 0) {
		for i := range size {
			folds[XYDir{x + size + i, -1, DirU}] = XYDir{x - size + i, 4*size - 1, DirU}
			folds[XYDir{x - size + i, 4 * size, DirD}] = XYDir{x + size + i, 0, DirD}
		}
	}

	// connect R to F, sample input
	if onMap(x, size) && onMap(x, 2*size) && onMap(x+size, 2*size) {
		for i := range size {
			folds[XYDir{x + size + i, 2*size - 1, DirU}] = XYDir{x + size - 1, 2*size - 1 - i, DirL}
			folds[XYDir{x + size, 2*size - 1 - i, DirR}] = XYDir{x + size + i, 2 * size, DirD}
		}
	}

	// connect R to F, my input
	if onMap(x, size) && onMap(x+size, 0) {
		for i := range size {
			folds[XYDir{x + size + i, size, DirD}] = XYDir{x + size - 1, size + i, DirL}
			folds[XYDir{x + size, size + i, DirR}] = XYDir{x + size + i, size - 1, DirU}
		}
	}

	return folds
}

func (aoc *aoc202222) part2(size int, input *Input) string {
	grid, moves := aoc.parse(input)
	folds := aoc.foldCube(size, grid)

	xydir := XYDir{0, 0, DirR}
	for grid[ToXY(xydir)] != '.' {
		xydir = AdvanceXYDir(xydir, 1)
	}

	for count, turn := range moves {
	advanceLoop:
		for range count {
			nxydir := AdvanceXYDir(xydir, 1)
			switch grid[ToXY(nxydir)] {
			case '#':
				break advanceLoop
			case '.':
				xydir = nxydir
			default:
				nxydir := folds[nxydir]
				switch grid[ToXY(nxydir)] {
				case '#':
					break advanceLoop
				case '.':
					xydir = nxydir
				default:
					panic("?")
				}
			}
		}
		xydir = turn(xydir)
	}
	return aoc.result(xydir)
}

func (aoc *aoc202222) Test2(input *Input) string {
	return aoc.part2(4, input)
}

func (aoc *aoc202222) Part2(input *Input) string {
	return aoc.part2(50, input)
}
