package main

import (
	"container/list"
)

func init() {
	Register(&aoc202415{
		AOC: AOC{
			Day:           15,
			InputFilename: "../2024/input/15.txt",
			Tests: []Test{
				Test{
					Input: `##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
`,
					Part1: "10092",
					Part2: "9021",
				},
				Test{
					Input: `########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
`,
					Part1: "2028",
					Part2: "",
				},
			},
		},
	})
}

type aoc202415 struct {
	AOC
}

func (aoc *aoc202415) parseGrid(input *Input) ([2]int, map[[2]int]byte) {
	grid := map[[2]int]byte{}
	start := [2]int{}
	x := 0
	y := 0
	for {
		ch, ok := input.Char()
		if !ok {
			panic("bad input")
		}
		switch ch {
		case '\n':
			if x == 0 {
				return start, grid
			}
			y++
			x = 0
		case '@':
			grid[[2]int{x, y}] = '.'
			start[0] = x
			start[1] = y
			x++
		default:
			grid[[2]int{x, y}] = ch
			x++
		}
	}
}

func (aoc *aoc202415) move(xy [2]int, grid map[[2]int]byte, movement byte) [2]int {
	dxy := [2]int{}
	switch movement {
	case '<':
		dxy[0] = -1
	case '>':
		dxy[0] = 1
	case '^':
		dxy[1] = -1
	case 'v':
		dxy[1] = 1
	default:
		return xy
	}
	dest := xy
forward:
	for {
		dest[0] += dxy[0]
		dest[1] += dxy[1]
		switch grid[dest] {
		case '#':
			return xy
		case '.':
			break forward
		case 'O':
		default:
			panic("oops")
		}
	}
	for {
		next := [2]int{dest[0] - dxy[0], dest[1] - dxy[1]}
		if next == xy {
			grid[dest] = '.'
			return dest
		}
		grid[dest] = 'O'
		dest = next
	}
}

func (aoc *aoc202415) sumGPS(grid map[[2]int]byte) int {
	sum := 0
	for xy, ch := range grid {
		if ch == 'O' {
			sum += xy[0] + 100*xy[1]
		}
	}
	return sum
}

func (aoc *aoc202415) p(xy [2]int, grid map[[2]int]byte) {
	w := -1
	h := -1
	for i := range grid {
		w = max(w, i[0])
		h = max(h, i[1])
	}
	w++
	h++
	for y := range h {
		for x := range w {
			if [2]int{x, y} == xy {
				print("@")
			} else {
				print(string([]byte{grid[[2]int{x, y}]}))
			}
		}
		println()
	}
}

func (aoc *aoc202415) Part1(input *Input) string {
	xy, grid := aoc.parseGrid(input)
	for movement, ok := input.Char(); ok; movement, ok = input.Char() {
		xy = aoc.move(xy, grid, movement)
	}
	return IntResult(aoc.sumGPS(grid))
}

func (aoc *aoc202415) parseGrid2(input *Input) ([2]int, map[[2]int]byte) {
	grid := map[[2]int]byte{}
	start := [2]int{}
	x := 0
	y := 0
	for {
		ch, ok := input.Char()
		if !ok {
			panic("bad input")
		}
		switch ch {
		case '\n':
			if x == 0 {
				return start, grid
			}
			y++
			x = 0
		case '@':
			grid[[2]int{2 * x, y}] = '.'
			grid[[2]int{2*x + 1, y}] = '.'
			start[0] = 2 * x
			start[1] = y
			x++
		case 'O':
			grid[[2]int{2 * x, y}] = 'O'
			grid[[2]int{2*x + 1, y}] = ']'
			x++
		default:
			grid[[2]int{2 * x, y}] = ch
			grid[[2]int{2*x + 1, y}] = ch
			x++
		}
	}
}

func (aoc *aoc202415) move2(xy [2]int, grid map[[2]int]byte, movement byte) [2]int {
	switch movement {
	case '<':
		return aoc.move2x(xy, grid, -1)
	case '>':
		return aoc.move2x(xy, grid, 1)
	case '^':
		return aoc.move2y(xy, grid, -1)
	case 'v':
		return aoc.move2y(xy, grid, 1)
	default:
		return xy
	}
}

func (aoc *aoc202415) move2x(xy [2]int, grid map[[2]int]byte, dx int) [2]int {
	dest := xy
forward:
	for {
		dest[0] += dx
		switch grid[dest] {
		case 'O', ']':
		case '#':
			return xy
		case '.':
			break forward
		default:
			panic("oops")
		}
	}
	for {
		next := [2]int{dest[0] - dx, dest[1]}
		if next == xy {
			grid[dest] = '.'
			return dest
		}
		grid[dest] = grid[next]
		dest = next
	}
}

func (aoc *aoc202415) move2y(xy [2]int, grid map[[2]int]byte, dy int) [2]int {
	tomove := map[[2]int]byte{}
	queue := list.New()
	next := [2]int{xy[0], xy[1] + dy}
forward:
	for {
		if _, ok := tomove[next]; ok {
			if queue.Len() == 0 {
				break forward
			}
			e := queue.Front()
			queue.Remove(e)
			next = e.Value.([2]int)
			continue forward
		}
		switch grid[next] {
		case '#':
			return xy
		case 'O':
			right := [2]int{next[0] + 1, next[1]}
			if _, ok := tomove[right]; !ok {
				queue.PushBack(right)
			}
			behind := [2]int{next[0], next[1] - dy}
			if _, ok := tomove[behind]; !ok {
				tomove[behind] = '.'
			}
			tomove[next] = 'O'
			next[1] += dy
		case ']':
			left := [2]int{next[0] - 1, next[1]}
			if _, ok := tomove[left]; !ok {
				queue.PushBack(left)
			}
			behind := [2]int{next[0], next[1] - dy}
			if _, ok := tomove[behind]; !ok {
				tomove[behind] = '.'
			}
			tomove[next] = ']'
			next[1] += dy
		case '.':
			if queue.Len() == 0 {
				break forward
			}
			e := queue.Front()
			queue.Remove(e)
			next = e.Value.([2]int)
			continue forward
		default:
			panic("oops")
		}
	}
	for xy, ch := range tomove {
		grid[[2]int{xy[0], xy[1] + dy}] = ch
	}
	return [2]int{xy[0], xy[1] + dy}
}

func (aoc *aoc202415) Part2(input *Input) string {
	xy, grid := aoc.parseGrid2(input)
	for movement, ok := input.Char(); ok; movement, ok = input.Char() {
		xy = aoc.move2(xy, grid, movement)
	}
	return IntResult(aoc.sumGPS(grid))
}
