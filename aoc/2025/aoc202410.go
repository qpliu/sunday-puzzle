package main

func init() {
	Register(&aoc202410{
		AOC: AOC{
			Day:           10,
			InputFilename: "../2024/input/10.txt",
			Tests: []Test{
				Test{
					Input: `0123
1234
8765
9876
`,
					Part1: "1",
					Part2: "",
				},
				Test{
					Input: `...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9
`,
					Part1: "2",
					Part2: "",
				},
				Test{
					Input: `..90..9
...1.98
...2..7
6543456
765.987
876....
987....
`,
					Part1: "4",
					Part2: "",
				},
				Test{
					Input: `10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01
`,
					Part1: "3",
					Part2: "",
				},
				Test{
					Input: `89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
`,
					Part1: "36",
					Part2: "81",
				},
				Test{
					Input: `.....0.
..4321.
..5..2.
..6543.
..7..4.
..8765.
..9....
`,
					Part1: "",
					Part2: "3",
				},
				Test{
					Input: `..90..9
...1.98
...2..7
6543456
765.987
876....
987....
`,
					Part1: "",
					Part2: "13",
				},
				Test{
					Input: `012345
123456
234567
345678
4.6789
56789.
`,
					Part1: "",
					Part2: "227",
				},
			},
		},
	})
}

type aoc202410 struct {
	AOC
}

func (aoc *aoc202410) score(c byte, xy [2]int, grid map[[2]int]byte, memo map[[2]int]map[[2]int]bool) map[[2]int]bool {
	if s, ok := memo[xy]; ok {
		return s
	}
	s := map[[2]int]bool{}
	memo[xy] = s
	if c == '9' {
		s[xy] = true
		return s
	}
	for _, nxy := range [][2]int{
		[2]int{xy[0] + 1, xy[1]},
		[2]int{xy[0] - 1, xy[1]},
		[2]int{xy[0], xy[1] + 1},
		[2]int{xy[0], xy[1] - 1}} {
		if c+1 == grid[nxy] {
			for summit := range aoc.score(c+1, nxy, grid, memo) {
				s[summit] = true
			}
		}
	}
	return s
}

func (aoc *aoc202410) Part1(input *Input) string {
	_, _, grid := input.Grid()
	memo := map[[2]int]map[[2]int]bool{}
	result := 0
	for xy, c := range grid {
		if c == '0' {
			result += len(aoc.score(c, xy, grid, memo))
		}
	}
	return IntResult(result)
}

func (aoc *aoc202410) rating(c byte, xy [2]int, grid map[[2]int]byte, memo map[[2]int]int) int {
	if c == '0' {
		return 1
	}
	if rating, ok := memo[xy]; ok {
		return rating
	}
	rating := 0
	for _, nxy := range [][2]int{
		[2]int{xy[0] + 1, xy[1]},
		[2]int{xy[0] - 1, xy[1]},
		[2]int{xy[0], xy[1] + 1},
		[2]int{xy[0], xy[1] - 1}} {
		if c-1 == grid[nxy] {
			rating += aoc.rating(c-1, nxy, grid, memo)
		}
	}
	memo[xy] = rating
	return rating
}

func (aoc *aoc202410) Part2(input *Input) string {
	_, _, grid := input.Grid()
	memo := map[[2]int]int{}
	result := 0
	for xy, c := range grid {
		if c == '9' {
			result += aoc.rating(c, xy, grid, memo)
		}
	}
	return IntResult(result)
}
