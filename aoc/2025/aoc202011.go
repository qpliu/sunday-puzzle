package main

func init() {
	Register(&aoc202011{
		AOC: AOC{
			Day:           11,
			InputFilename: "../2020/input/11.txt",
			Tests: []Test{
				Test{
					Input: `L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
`,
					Part1: "37",
					Part2: "26",
				},
			},
		},
	})
}

type aoc202011 struct {
	AOC
}

func (aoc *aoc202011) parse(part1 bool, input *Input) [][][2]int {
	w, h, grid := input.Grid()
	seats := [][][2]int{}
	for xy, ch := range grid {
		if ch != 'L' {
			continue
		}
		seat := [][2]int{xy}
		if part1 {
			for _, n := range [...]XY{
				AdvanceXY(xy, DirR, 1),
				AdvanceXY(xy, DirR|DirD, 1),
				AdvanceXY(xy, DirD, 1),
				AdvanceXY(xy, DirD|DirL, 1),
				AdvanceXY(xy, DirL, 1),
				AdvanceXY(xy, DirL|DirU, 1),
				AdvanceXY(xy, DirU, 1),
				AdvanceXY(xy, DirU|DirR, 1),
			} {
				if grid[n] == 'L' {
					seat = append(seat, n)
				}
			}
		} else {
			for _, dir := range [...]int{DirR, DirR | DirD, DirD, DirD | DirL, DirL, DirL | DirU, DirU, DirU | DirR} {
				n := AdvanceXY(xy, dir, 1)
				for n[0] >= 0 && n[0] < w && n[1] >= 0 && n[1] < h {
					if grid[n] == 'L' {
						seat = append(seat, n)
						break
					}
					n = AdvanceXY(n, dir, 1)
				}
			}
		}
		seats = append(seats, seat)
	}
	return seats
}

func (aoc *aoc202011) result(part1 bool, input *Input) string {
	threshold := 5
	if part1 {
		threshold = 4
	}
	seats := aoc.parse(part1, input)
	area := map[XY]bool{}
	tmp := map[XY]bool{}
	changed := true
	for changed {
		changed = false
		clear(tmp)
		for _, seat := range seats {
			count := 0
			for _, n := range seat[1:] {
				if area[n] {
					count++
				}
			}
			if area[seat[0]] {
				if count >= threshold {
					changed = true
				} else {
					tmp[seat[0]] = true
				}
			} else {
				if count == 0 {
					changed = true
					tmp[seat[0]] = true
				}
			}
		}
		area, tmp = tmp, area
	}
	return IntResult(len(area))
}

func (aoc *aoc202011) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202011) Part2(input *Input) string {
	return aoc.result(false, input)
}
