package main

func init() {
	Register(&aoc202314{
		AOC: AOC{
			Day:           14,
			InputFilename: "../2023/input/14.txt",
			Tests: []Test{
				Test{
					Input: `O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
`,
					Part1: "136",
					Part2: "64",
				},
			},
		},
	})
}

type aoc202314 struct {
	AOC
}

func (aoc *aoc202314) parse(input *Input) (int, int, map[[2]int]byte) {
	w, h := -1, -1
	grid := map[[2]int]byte{}
	x, y := 0, 0
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		if ch == '\n' {
			x = 0
			y++
			continue
		}
		w = max(w, x)
		h = max(h, y)
		if ch == '#' || ch == 'O' {
			grid[[2]int{x, y}] = ch
		}
		x++
	}
	return w + 1, h + 1, grid
}

func (aoc *aoc202314) load(h int, grid map[[2]int]byte) int {
	load := 0
	for xy, ch := range grid {
		if ch == 'O' {
			load += h - xy[1]
		}
	}
	return load
}

func (aoc *aoc202314) hload(grid map[[2]int]byte) int {
	hload := 0
	for xy, ch := range grid {
		if ch == 'O' {
			hload += xy[0] + 1
		}
	}
	return hload
}

func (aoc *aoc202314) tiltN(h, w int, grid map[[2]int]byte) {
xloop:
	for x := range w {
		desty := 0
		srcy := 1
		for {
			for grid[[2]int{x, desty}] != 0 {
				desty++
			}
			srcy = max(srcy, desty+1)
			for grid[[2]int{x, srcy}] == 0 {
				srcy++
				if srcy >= h {
					continue xloop
				}
			}
			if grid[[2]int{x, srcy}] == 'O' {
				grid[[2]int{x, desty}] = 'O'
				delete(grid, [2]int{x, srcy})
				srcy++
			} else {
				desty = srcy + 1
			}
		}
	}
}

func (aoc *aoc202314) tiltS(h, w int, grid map[[2]int]byte) {
xloop:
	for x := range w {
		desty := h - 1
		srcy := h - 2
		for {
			for grid[[2]int{x, desty}] != 0 {
				desty--
			}
			srcy = min(srcy, desty-1)
			for grid[[2]int{x, srcy}] == 0 {
				srcy--
				if srcy < 0 {
					continue xloop
				}
			}
			if grid[[2]int{x, srcy}] == 'O' {
				grid[[2]int{x, desty}] = 'O'
				delete(grid, [2]int{x, srcy})
				srcy--
			} else {
				desty = srcy - 1
			}
		}
	}
}

func (aoc *aoc202314) tiltE(h, w int, grid map[[2]int]byte) {
yloop:
	for y := range h {
		destx := w - 1
		srcx := w - 2
		for {
			for grid[[2]int{destx, y}] != 0 {
				destx--
			}
			srcx = min(srcx, destx-1)
			for grid[[2]int{srcx, y}] == 0 {
				srcx--
				if srcx < 0 {
					continue yloop
				}
			}
			if grid[[2]int{srcx, y}] == 'O' {
				grid[[2]int{destx, y}] = 'O'
				delete(grid, [2]int{srcx, y})
				srcx--
			} else {
				destx = srcx - 1
			}
		}
	}
}

func (aoc *aoc202314) tiltW(h, w int, grid map[[2]int]byte) {
yloop:
	for y := range h {
		destx := 0
		srcx := 1
		for {
			for grid[[2]int{destx, y}] != 0 {
				destx++
			}
			srcx = max(srcx, destx+1)
			for grid[[2]int{srcx, y}] == 0 {
				srcx++
				if srcx >= w {
					continue yloop
				}
			}
			if grid[[2]int{srcx, y}] == 'O' {
				grid[[2]int{destx, y}] = 'O'
				delete(grid, [2]int{srcx, y})
				srcx++
			} else {
				destx = srcx + 1
			}
		}
	}
}

func (aoc *aoc202314) Part1(input *Input) string {
	h, w, grid := aoc.parse(input)
	aoc.tiltN(h, w, grid)
	return IntResult(aoc.load(h, grid))
}

func (aoc *aoc202314) Part2(input *Input) string {
	h, w, grid := aoc.parse(input)
	loads := [][2]int{}
	byLoad := map[[2]int]int{}
	for t := 0; ; t++ {
		load := [2]int{aoc.load(h, grid), aoc.hload(grid)}
		if tstart, ok := byLoad[load]; ok {
			return IntResult(loads[(1000000000-tstart)%(t-tstart)+tstart][0])
		}
		loads = append(loads, load)
		byLoad[load] = t
		aoc.tiltN(h, w, grid)
		aoc.tiltW(h, w, grid)
		aoc.tiltS(h, w, grid)
		aoc.tiltE(h, w, grid)
	}
}
