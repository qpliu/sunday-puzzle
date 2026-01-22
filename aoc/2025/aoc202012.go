package main

func init() {
	Register(&aoc202012{
		AOC: AOC{
			Day:           12,
			InputFilename: "../2020/input/12.txt",
			Tests: []Test{
				Test{
					Input: `F10
N3
F7
R90
F11
`,
					Part1: "25",
					Part2: "286",
				},
			},
		},
	})
}

type aoc202012 struct {
	AOC
}

func (aoc *aoc202012) Part1(input *Input) string {
	xy := XY{}
	dir := DirR
	for move := range input.Words() {
		n, _ := InputString(move[1:]).Int()
		switch move[0] {
		case 'N':
			xy = AdvanceXY(xy, DirU, n)
		case 'S':
			xy = AdvanceXY(xy, DirD, n)
		case 'E':
			xy = AdvanceXY(xy, DirR, n)
		case 'W':
			xy = AdvanceXY(xy, DirL, n)
		case 'L':
			for n > 0 {
				dir = TurnL(dir)
				n -= 90
			}
		case 'R':
			for n > 0 {
				dir = TurnR(dir)
				n -= 90
			}
		case 'F':
			xy = AdvanceXY(xy, dir, n)
		default:
			panic("bad input")
		}
	}
	return IntResult(max(xy[0], -xy[0]) + max(xy[1], -xy[1]))
}

func (aoc *aoc202012) Part2(input *Input) string {
	xy := XY{}
	wp := XY{10, -1}
	for move := range input.Words() {
		n, _ := InputString(move[1:]).Int()
		switch move[0] {
		case 'N':
			wp = AdvanceXY(wp, DirU, n)
		case 'S':
			wp = AdvanceXY(wp, DirD, n)
		case 'E':
			wp = AdvanceXY(wp, DirR, n)
		case 'W':
			wp = AdvanceXY(wp, DirL, n)
		case 'L':
			for n > 0 {
				wp = XY{wp[1], -wp[0]}
				n -= 90
			}
		case 'R':
			for n > 0 {
				wp = XY{-wp[1], wp[0]}
				n -= 90
			}
		case 'F':
			xy = XY{xy[0] + n*wp[0], xy[1] + n*wp[1]}
		default:
			panic("bad input")
		}
	}
	return IntResult(max(xy[0], -xy[0]) + max(xy[1], -xy[1]))
}
