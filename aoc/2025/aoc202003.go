package main

func init() {
	Register(&aoc202003{
		AOC: AOC{
			Day:           3,
			InputFilename: "../2020/input/03.txt",
			Tests: []Test{
				Test{
					Input: `..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
`,
					Part1: "7",
					Part2: "336",
				},
			},
		},
	})
}

type aoc202003 struct {
	AOC
}

func (aoc *aoc202003) Part1(input *Input) string {
	w, h, grid := input.Grid()
	result := 0
	for y := range h {
		if grid[XY{(3 * y) % w, y}] == '#' {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202003) Part2(input *Input) string {
	w, h, grid := input.Grid()
	result := 1
	for _, slope := range [...]int{1, 3, 5, 7} {
		n := 0
		for y := range h {
			if grid[XY{(slope * y) % w, y}] == '#' {
				n++
			}
		}
		result *= n
	}
	n := 0
	for y := range h {
		if y%2 == 0 && grid[XY{(y / 2) % w, y}] == '#' {
			n++
		}
	}
	result *= n
	return IntResult(result)
}
