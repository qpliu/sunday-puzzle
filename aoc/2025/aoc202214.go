package main

func init() {
	Register(&aoc202214{
		AOC: AOC{
			Day:           14,
			InputFilename: "../2022/input/14.txt",
			Tests: []Test{
				Test{
					Input: `498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
`,
					Part1: "24",
					Part2: "93",
				},
			},
		},
	})
}

type aoc202214 struct {
	AOC
}

func (aoc *aoc202214) parse(input *Input) (int, map[XY]bool) {
	grid := map[XY]bool{}
	ymax := 0
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		in := InputString(line)
		x, _ := in.Int()
		y, _ := in.Int()
		ymax = max(ymax, y)
		for {
			x2, _ := in.Int()
			y2, ok := in.Int()
			if !ok {
				break
			}
			if y == y2 {
				dx := 1
				if x2 < x {
					dx = -1
				}
				for ; x != x2; x += dx {
					grid[XY{x, y}] = true
				}
			} else if x == x2 {
				dy := 1
				if y2 < y {
					dy = -1
				}
				for ; y != y2; y += dy {
					grid[XY{x, y}] = true
				}
			} else {
				panic("bad input")
			}
			grid[XY{x, y}] = true
			ymax = max(ymax, y)
		}
	}
	return ymax, grid
}

func (aoc *aoc202214) result(part1 bool, input *Input) string {
	ymax, grid := aoc.parse(input)
	ymax++
	nrock := len(grid)

	type flow struct {
		x, y int
		last *flow
	}

	sand := &flow{500, 0, nil}
	for sand != nil {
		x, y := sand.x, sand.y
		if y == ymax {
			if part1 {
				break
			}
			grid[XY{x, y}] = true
			sand = sand.last
			continue
		}
		if !grid[XY{x, y + 1}] {
			sand = &flow{x, y + 1, sand}
			continue
		}
		if !grid[XY{x - 1, y + 1}] {
			sand = &flow{x - 1, y + 1, sand}
			continue
		}
		if !grid[XY{x + 1, y + 1}] {
			sand = &flow{x + 1, y + 1, sand}
			continue
		}
		grid[XY{x, y}] = true
		sand = sand.last
	}

	return IntResult(len(grid) - nrock)
}

func (aoc *aoc202214) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202214) Part2(input *Input) string {
	return aoc.result(false, input)
}
