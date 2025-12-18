package main

func init() {
	Register(&aoc202404{
		AOC: AOC{
			Day:           4,
			InputFilename: "../2024/input/04.txt",
			Tests: []Test{
				Test{
					Input: `MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
`,
					Part1: "18",
					Part2: "9",
				},
			},
		},
	})
}

type aoc202404 struct {
	AOC
}

func (aoc *aoc202404) Part1(input *Input) string {
	_, _, grid := input.Grid()
	result := 0
	for xy, ch := range grid {
		if ch != 'X' {
			continue
		}
		x := xy[0]
		y := xy[1]
		for _, dx := range [3]int{-1, 0, 1} {
			for _, dy := range [3]int{-1, 0, 1} {
				if grid[[2]int{x + dx, y + dy}] == 'M' && grid[[2]int{x + 2*dx, y + 2*dy}] == 'A' && grid[[2]int{x + 3*dx, y + 3*dy}] == 'S' {
					result++
				}
			}
		}
	}
	return IntResult(result)
}

func (aoc aoc202404) mas(a, b byte) bool {
	return (a == 'M' && b == 'S') || (a == 'S' && b == 'M')
}

func (aoc aoc202404) Part2(input *Input) string {
	_, _, grid := input.Grid()
	result := 0
	for xy, ch := range grid {
		if ch != 'A' {
			continue
		}
		x := xy[0]
		y := xy[1]
		if aoc.mas(grid[[2]int{x - 1, y - 1}], grid[[2]int{x + 1, y + 1}]) && aoc.mas(grid[[2]int{x + 1, y - 1}], grid[[2]int{x - 1, y + 1}]) {
			result++
		}
	}
	return IntResult(result)
}
