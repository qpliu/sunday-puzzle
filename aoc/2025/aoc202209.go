package main

func init() {
	Register(&aoc202209{
		AOC: AOC{
			Day:           9,
			InputFilename: "../2022/input/09.txt",
			Tests: []Test{
				Test{
					Input: `R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
`,
					Part1: "13",
					Part2: "1",
				},
				Test{
					Input: `R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20
`,
					Part1: "",
					Part2: "36",
				},
			},
		},
	})
}

type aoc202209 struct {
	AOC
}

func (aoc *aoc202209) next(input *Input) (int, int, bool) {
	dir, _ := input.Word()
	count, ok := input.Int()
	if !ok {
		return 0, 0, false
	}
	switch dir {
	case "R":
		return DirR, count, true
	case "D":
		return DirD, count, true
	case "L":
		return DirL, count, true
	case "U":
		return DirU, count, true
	default:
		panic("bad input")
	}
}

func (aoc *aoc202209) follow(head, tail XY, dir, count int) (int, int) {
	// returns direction to move diagonally once if needed,
	// and the number of steps to move subsequently in dir
	if head == tail {
		return 0, max(count-1, 0)
	}
	switch dir {
	case DirR:
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}:
			return 0, count
		case [2]int{1, 1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirR | DirD, count - 1
			}
		case [2]int{0, 1}:
			if count < 2 {
				return 0, 0
			} else {
				return DirR | DirD, count - 2
			}
		case [2]int{-1, 1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirR | DirD, count - 3
			}
		case [2]int{-1, 0}:
			return 0, max(0, count-2)
		case [2]int{-1, -1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirR | DirU, count - 3
			}
		case [2]int{0, -1}:
			if count < 2 {
				return 0, 0
			} else {
				return DirR | DirU, count - 2
			}
		case [2]int{1, -1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirR | DirU, count - 1
			}
		default:
			println("<", head[0], ",", head[1], ":", tail[0], ",", tail[1], ">") // DO NOT COMMIT
			panic("?")
		}
	case DirD:
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}:
			if count < 2 {
				return 0, 0
			} else {
				return DirR | DirD, count - 2
			}
		case [2]int{1, 1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirR | DirD, count - 1
			}
		case [2]int{0, 1}:
			return 0, count
		case [2]int{-1, 1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirL | DirD, count - 1
			}
		case [2]int{-1, 0}:
			if count < 2 {
				return 0, 0
			} else {
				return DirL | DirD, count - 2
			}
		case [2]int{-1, -1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirL | DirD, count - 3
			}
		case [2]int{0, -1}:
			return 0, max(0, count-2)
		case [2]int{1, -1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirR | DirD, count - 3
			}
		default:
			panic("?")
		}
	case DirL:
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}:
			return 0, max(0, count-2)
		case [2]int{1, 1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirL | DirD, count - 3
			}
		case [2]int{0, 1}:
			if count < 2 {
				return 0, 0
			} else {
				return DirL | DirD, count - 2
			}
		case [2]int{-1, 1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirL | DirD, count - 1
			}
		case [2]int{-1, 0}:
			return 0, count
		case [2]int{-1, -1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirL | DirU, count - 1
			}
		case [2]int{0, -1}:
			if count < 2 {
				return 0, 0
			} else {
				return DirL | DirU, count - 2
			}
		case [2]int{1, -1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirL | DirU, count - 3
			}
		default:
			panic("?")
		}
	case DirU:
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}:
			if count < 2 {
				return 0, 0
			} else {
				return DirR | DirU, count - 2
			}
		case [2]int{1, 1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirR | DirU, count - 3
			}
		case [2]int{0, 1}:
			return 0, max(0, count-2)
		case [2]int{-1, 1}:
			if count < 3 {
				return 0, 0
			} else {
				return DirL | DirU, count - 3
			}
		case [2]int{-1, 0}:
			if count < 2 {
				return 0, 0
			} else {
				return DirL | DirU, count - 2
			}
		case [2]int{-1, -1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirL | DirU, count - 1
			}
		case [2]int{0, -1}:
			return 0, count
		case [2]int{1, -1}:
			if count < 1 {
				return 0, 0
			} else {
				return DirR | DirU, count - 1
			}
		default:
			panic("?")
		}
	case DirR | DirD:
		if count != 1 {
			panic("?")
		}
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}, [2]int{1, 1}, [2]int{0, 1}:
			return dir, 0
		case [2]int{-1, 1}:
			return DirD, 0
		case [2]int{1, -1}:
			return DirR, 0
		default:
			return 0, 0
		}
	case DirL | DirD:
		if count != 1 {
			panic("?")
		}
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{-1, 0}, [2]int{-1, 1}, [2]int{0, 1}:
			return dir, 0
		case [2]int{1, 1}:
			return DirD, 0
		case [2]int{-1, -1}:
			return DirL, 0
		default:
			return 0, 0
		}
	case DirL | DirU:
		if count != 1 {
			panic("?")
		}
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{-1, 0}, [2]int{-1, -1}, [2]int{0, -1}:
			return dir, 0
		case [2]int{1, -1}:
			return DirU, 0
		case [2]int{-1, 1}:
			return DirL, 0
		default:
			return 0, 0
		}
	case DirR | DirU:
		if count != 1 {
			panic("?")
		}
		switch [2]int{head[0] - tail[0], head[1] - tail[1]} {
		case [2]int{1, 0}, [2]int{1, -1}, [2]int{0, -1}:
			return dir, 0
		case [2]int{-1, -1}:
			return DirU, 0
		case [2]int{1, 1}:
			return DirR, 0
		default:
			return 0, 0
		}
	default:
		panic("?")
	}
}

func (aoc *aoc202209) visit(xy XY, dir, count int, visited map[XY]bool) XY {
	for range count {
		xy = AdvanceXY(xy, dir, 1)
		visited[xy] = true
	}
	return xy
}

func (aoc *aoc202209) Part1(input *Input) string {
	visited := map[XY]bool{}
	head, tail := XY{}, XY{}
	visited[tail] = true
	for dir, count, ok := aoc.next(input); ok; dir, count, ok = aoc.next(input) {
		followDiagonal, followCount := aoc.follow(head, tail, dir, count)
		head = AdvanceXY(head, dir, count)
		if followDiagonal != 0 {
			tail = aoc.visit(tail, followDiagonal, 1, visited)
		}
		tail = aoc.visit(tail, dir, followCount, visited)
	}
	return IntResult(len(visited))
}

func (aoc *aoc202209) Part2(input *Input) string {
	const n = 10
	visited := map[XY]bool{}
	knots := [n]XY{}
	visited[knots[n-1]] = true

	var move func(head, dir, count int)
	move = func(head, dir, count int) {
		followDiagonal, followCount := aoc.follow(knots[head], knots[head+1], dir, count)
		knots[head] = AdvanceXY(knots[head], dir, count)
		if head == n-2 {
			if followDiagonal != 0 {
				knots[head+1] = aoc.visit(knots[head+1], followDiagonal, 1, visited)
			}
			knots[head+1] = aoc.visit(knots[head+1], dir, followCount, visited)
		} else {
			if followDiagonal != 0 {
				move(head+1, followDiagonal, 1)
			}
			if followCount > 0 {
				move(head+1, dir, followCount)
			}
		}
	}

	for dir, count, ok := aoc.next(input); ok; dir, count, ok = aoc.next(input) {
		move(0, dir, count)
	}
	return IntResult(len(visited))
}
