package main

func init() {
	Register(&aoc202504{
		AOC: AOC{
			Day: 4,
			Tests: []Test{
				Test{
					Input: `..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
`,
					Part1: "13",
					Part2: "43",
				},
			},
		},
	})
}

type aoc202504 struct {
	AOC
}

func (aoc *aoc202504) parse(input *Input) map[[2]int]bool {
	grid := map[[2]int]bool{}
	x := 0
	y := 0
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		switch ch {
		case '@':
			grid[[2]int{x, y}] = true
			x++
		case '\n':
			x = 0
			y++
		default:
			x++
		}
	}
	return grid
}

func (aoc *aoc202504) neighbors(xy [2]int) [8][2]int {
	x := xy[0]
	y := xy[1]
	return [8][2]int{
		[2]int{x - 1, y - 1}, [2]int{x, y - 1}, [2]int{x + 1, y - 1},
		[2]int{x - 1, y}, [2]int{x + 1, y},
		[2]int{x - 1, y + 1}, [2]int{x, y + 1}, [2]int{x + 1, y + 1},
	}
}

func (aoc *aoc202504) removeRolls(grid, candidates map[[2]int]bool) map[[2]int]bool {
	removals := map[[2]int]bool{}
	for xy := range candidates {
		neighborCount := 0
		for _, neighbor := range aoc.neighbors(xy) {
			if grid[neighbor] {
				neighborCount++
			}
		}
		if neighborCount < 4 {
			removals[xy] = true
		}
	}
	for xy := range removals {
		delete(grid, xy)
	}
	nextCandidates := map[[2]int]bool{}
	for xy := range removals {
		for _, neighbor := range aoc.neighbors(xy) {
			if grid[neighbor] {
				nextCandidates[neighbor] = true
			}
		}
	}
	return nextCandidates
}

func (aoc *aoc202504) Part1(input *Input) string {
	grid := aoc.parse(input)
	initialCount := len(grid)
	aoc.removeRolls(grid, grid)
	return IntResult(initialCount - len(grid))
}

func (aoc *aoc202504) Part2(input *Input) string {
	grid := aoc.parse(input)
	initialCount := len(grid)
	for candidates := aoc.removeRolls(grid, grid); len(candidates) > 0; candidates = aoc.removeRolls(grid, candidates) {
	}
	return IntResult(initialCount - len(grid))
}
