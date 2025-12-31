package main

func init() {
	Register(&aoc202316{
		AOC: AOC{
			Day:           16,
			InputFilename: "../2023/input/16.txt",
			Tests: []Test{
				Test{
					Input: `.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
`,
					Part1: "46",
					Part2: "",
				},
			},
		},
	})
}

type aoc202316 struct {
	AOC
}

func (aoc *aoc202316) activates(xyDir XYDir, grid map[XY]byte) int {
	active := map[XY]int{}
	queue := NewQueue[XYDir]()
	for {
		xy := ToXY(xyDir)
		dir := xyDir[2]
		if active[xy]&dir != 0 || grid[xy] == 0 {
			if queue.Empty() {
				return len(active)
			}
			xyDir = queue.Dequeue()
			continue
		}
		active[xy] |= dir
		switch grid[xy] {
		case '.':
		case '|':
			if dir == DirR || dir == DirL {
				xyDir[2] = DirD
				xyd := xyDir
				xyd[2] = DirU
				queue.Enqueue(AdvanceXYDir(xyd, 1))
			}
		case '-':
			if dir == DirD || dir == DirU {
				xyDir[2] = DirR
				xyd := xyDir
				xyd[2] = DirL
				queue.Enqueue(AdvanceXYDir(xyd, 1))
			}
		case '/':
			switch dir {
			case DirR:
				xyDir[2] = DirU
			case DirD:
				xyDir[2] = DirL
			case DirL:
				xyDir[2] = DirD
			case DirU:
				xyDir[2] = DirR
			default:
				panic("?")
			}
		case '\\':
			switch dir {
			case DirR:
				xyDir[2] = DirD
			case DirD:
				xyDir[2] = DirR
			case DirL:
				xyDir[2] = DirU
			case DirU:
				xyDir[2] = DirL
			default:
				panic("?")
			}
		default:
			panic("bad input")
		}
		xyDir = AdvanceXYDir(xyDir, 1)
	}
}

func (aoc *aoc202316) Part1(input *Input) string {
	_, _, grid := input.Grid()
	return IntResult(aoc.activates(XYDir{0, 0, DirR}, grid))
}

func (aoc *aoc202316) Part2(input *Input) string {
	w, h, grid := input.Grid()
	result := 0
	for x := range w {
		result = max(result, aoc.activates(XYDir{x, 0, DirD}, grid))
		result = max(result, aoc.activates(XYDir{x, h - 1, DirU}, grid))
	}
	for y := range h {
		result = max(result, aoc.activates(XYDir{0, y, DirR}, grid))
		result = max(result, aoc.activates(XYDir{w - 1, y, DirL}, grid))
	}
	return IntResult(result)
}
