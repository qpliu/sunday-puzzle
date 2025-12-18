package main

func init() {
	Register(&aoc202408{
		AOC: AOC{
			Day:           8,
			InputFilename: "../2024/input/08.txt",
			Tests: []Test{
				Test{
					Input: `............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
`,
					Part1: "14",
					Part2: "34",
				},
			},
		},
	})
}

type aoc202408 struct {
	AOC
}

func (aoc *aoc202408) parse(input *Input) (int, int, [][][2]int) {
	width, height, grid := input.Grid()
	byFreq := map[byte][][2]int{}
	for xy, ch := range grid {
		if ch != '.' {
			byFreq[ch] = append(byFreq[ch], xy)
		}
	}
	var groups [][][2]int
	for _, group := range byFreq {
		groups = append(groups, group)
	}
	return width, height, groups
}

func (aoc *aoc202408) markAntinodes(width, height int, group [][2]int, antinodes map[[2]int]bool) {
	for i, xy1 := range group {
		for _, xy2 := range group[i+1:] {
			dx := xy1[0] - xy2[0]
			dy := xy1[1] - xy2[1]
			if xy1[0]+dx >= 0 && xy1[0]+dx < width &&
				xy1[1]+dy >= 0 && xy1[1]+dy < height {
				antinodes[[2]int{xy1[0] + dx, xy1[1] + dy}] = true
			}
			if xy2[0]-dx >= 0 && xy2[0]-dx < width &&
				xy2[1]-dy >= 0 && xy2[1]-dy < height {
				antinodes[[2]int{xy2[0] - dx, xy2[1] - dy}] = true
			}
		}
	}
}

func (aoc *aoc202408) Part1(input *Input) string {
	width, height, groups := aoc.parse(input)
	result := map[[2]int]bool{}
	for _, group := range groups {
		aoc.markAntinodes(width, height, group, result)
	}
	return IntResult(len(result))
}

func (aoc *aoc202408) markResonances(width, height int, group [][2]int, antinodes map[[2]int]bool) {
	for i, xy1 := range group {
		for _, xy2 := range group[i+1:] {
			dx := xy1[0] - xy2[0]
			dy := xy1[1] - xy2[1]
			for n := 0; ; n++ {
				if xy1[0]+n*dx >= 0 && xy1[0]+n*dx < width &&
					xy1[1]+n*dy >= 0 && xy1[1]+n*dy < height {
					antinodes[[2]int{xy1[0] + n*dx, xy1[1] + n*dy}] = true
				} else {
					break
				}
			}
			for n := 1; ; n++ {
				if xy1[0]-n*dx >= 0 && xy1[0]-n*dx < width &&
					xy1[1]-n*dy >= 0 && xy1[1]-n*dy < height {
					antinodes[[2]int{xy1[0] - n*dx, xy1[1] - n*dy}] = true
				} else {
					break
				}
			}
		}
	}
}

func (aoc *aoc202408) Part2(input *Input) string {
	width, height, groups := aoc.parse(input)
	result := map[[2]int]bool{}
	for _, group := range groups {
		aoc.markResonances(width, height, group, result)
	}
	n1 := 0
	n2 := 0
	for _, b := range result {
		n1++
		if b {
			n2++
		}
	}
	return IntResult(len(result))
}
