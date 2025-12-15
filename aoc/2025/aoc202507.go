package main

func init() {
	Register(&aoc202507{
		AOC: AOC{
			Day: 7,
			Tests: []Test{
				Test{
					Input: `.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
`,
					Part1: "21",
					Part2: "40",
				},
			},
		},
	})
}

type aoc202507 struct {
	AOC
}

func (aoc *aoc202507) Part1(input *Input) string {
	result := 0
	width, height, manifold := input.Grid()
	beams := map[[2]int]bool{}
	for y := range height {
		for x := range width {
			switch manifold[[2]int{x, y}] {
			case 'S':
				beams[[2]int{x, y}] = true
			case '.':
				if beams[[2]int{x, y - 1}] ||
					(manifold[[2]int{x - 1, y}] == '^' && beams[[2]int{x - 1, y - 1}]) ||
					(manifold[[2]int{x + 1, y}] == '^' && beams[[2]int{x + 1, y - 1}]) {
					beams[[2]int{x, y}] = true
				}
			}
			if beams[[2]int{x, y}] && manifold[[2]int{x, y + 1}] == '^' {
				result++
			}
		}
	}
	return IntResult(result)
}

func (aoc *aoc202507) Part2(input *Input) string {
	width, height, manifold := input.Grid()
	beams := map[[2]int]int{}
	for y := range height {
		for x := range width {
			switch manifold[[2]int{x, y}] {
			case 'S':
				beams[[2]int{x, y}] = 1
			case '.':
				timelines := beams[[2]int{x, y - 1}]
				if manifold[[2]int{x - 1, y}] == '^' {
					timelines += beams[[2]int{x - 1, y - 1}]
				}
				if manifold[[2]int{x + 1, y}] == '^' {
					timelines += beams[[2]int{x + 1, y - 1}]
				}
				beams[[2]int{x, y}] = timelines
			}
		}
	}
	result := 0
	for x := range width {
		result += beams[[2]int{x, height - 1}]
	}
	return IntResult(result)
}
