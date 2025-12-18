package main

func init() {
	Register(&aoc202420{
		AOC: AOC{
			Day:           20,
			InputFilename: "../2024/input/20.txt",
			Tests: []Test{
				Test{
					Input: `###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############
`,
					Part1: "44",
					Part2: "285",
				},
			},
		},
	})
}

type aoc202420 struct {
	AOC
}

func (aoc *aoc202420) parse(input *Input) map[[2]int]int {
	_, _, grid := input.Grid()
	xy := [2]int{}
	for pxy, ch := range grid {
		if ch == 'S' {
			xy = pxy
			break
		}
	}
	track := map[[2]int]int{}
	t := 0
	for {
		track[xy] = t
		if grid[xy] == 'E' {
			return track
		}
		t++
		for _, nxy := range [4][2]int{
			[2]int{xy[0] + 1, xy[1]},
			[2]int{xy[0], xy[1] + 1},
			[2]int{xy[0] - 1, xy[1]},
			[2]int{xy[0], xy[1] - 1},
		} {
			if _, ok := track[nxy]; ok || grid[nxy] == '#' {
				continue
			}
			xy = nxy
			break
		}
	}
}

func (aoc *aoc202420) run(cheat, savings int, input *Input) int {
	track := aoc.parse(input)
	count := 0
	for xy, t := range track {
		for dx := range cheat + 1 {
			sxs := []int{-1, 1}
			if dx == 0 {
				sxs = []int{1}
			}
			for dy := range cheat + 1 - dx {
				sys := []int{-1, 1}
				if dy == 0 {
					sys = []int{1}
				}
				for _, sx := range sxs {
					for _, sy := range sys {
						t2, ok := track[[2]int{xy[0] + sx*dx, xy[1] + sy*dy}]
						if ok && t2-t-dx-dy >= savings {
							count++
						}
					}
				}
			}
		}
	}
	return count
}

func (aoc *aoc202420) Test1(input *Input) string {
	return IntResult(aoc.run(2, 2, input))
}

func (aoc *aoc202420) Part1(input *Input) string {
	return IntResult(aoc.run(2, 100, input))
}

func (aoc *aoc202420) Test2(input *Input) string {
	return IntResult(aoc.run(20, 50, input))
}

func (aoc *aoc202420) Part2(input *Input) string {
	return IntResult(aoc.run(20, 100, input))
}
