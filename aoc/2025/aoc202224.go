package main

func init() {
	Register(&aoc202224{
		AOC: AOC{
			Day:           24,
			InputFilename: "../2022/input/24.txt",
			Tests: []Test{
				Test{
					Input: `#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
`,
					Part1: "18",
					Part2: "54",
				},
			},
		},
	})
}

type aoc202224 struct {
	AOC
}

func (aoc *aoc202224) parse(input *Input) (XY, [][]BitSet256) {
	w, h, grid := input.Grid()
	rt := map[XY]bool{}
	dn := map[XY]bool{}
	lt := map[XY]bool{}
	up := map[XY]bool{}
	for xy, ch := range grid {
		switch ch {
		case '>':
			rt[xy] = true
		case 'v':
			dn[xy] = true
		case '<':
			lt[xy] = true
		case '^':
			up[xy] = true
		}
	}

	top := BitSet256{}
	for x := range w {
		if x != 1 {
			top = top.Add(x)
		}
	}
	bottom := BitSet256{}
	for x := range w {
		if x != w-2 {
			bottom = bottom.Add(x)
		}
	}
	walls := BitSet256{}.Add(0).Add(w - 1)

	valleys := [][]BitSet256{}
	for range LCM(w-2, h-2) {
		valley := []BitSet256{top}
		for y := 1; y <= h-2; y++ {
			r := walls
			for x := 1; x <= w-2; x++ {
				xy := XY{x, y}
				if rt[xy] || dn[xy] || lt[xy] || up[xy] {
					r = r.Add(x)
				}
			}
			valley = append(valley, r)
		}
		valleys = append(valleys, append(valley, bottom))

		nextRT := map[XY]bool{}
		for xy := range rt {
			if xy[0] == w-2 {
				xy[0] = 1
			} else {
				xy[0]++
			}
			nextRT[xy] = true
		}
		rt = nextRT

		nextDN := map[XY]bool{}
		for xy := range dn {
			if xy[1] == h-2 {
				xy[1] = 1
			} else {
				xy[1]++
			}
			nextDN[xy] = true
		}
		dn = nextDN

		nextLT := map[XY]bool{}
		for xy := range lt {
			if xy[0] == 1 {
				xy[0] = w - 2
			} else {
				xy[0]--
			}
			nextLT[xy] = true
		}
		lt = nextLT

		nextUP := map[XY]bool{}
		for xy := range up {
			if xy[1] == 1 {
				xy[1] = h - 2
			} else {
				xy[1]--
			}
			nextUP[xy] = true
		}
		up = nextUP
	}

	return XY{w - 2, h - 1}, valleys
}

func (aoc *aoc202224) Part1(input *Input) string {
	goal, valleys := aoc.parse(input)

	type path struct {
		xy XY
		t  int
	}

	start := []path{path{xy: XY{1, 0}}}

	done := func(p path) bool {
		return p.xy == goal
	}

	priority := func(p path) int {
		return -p.t - (goal[0] - p.xy[0]) - (goal[1] - p.xy[1])
	}

	state := func(p path) [3]int {
		return [3]int{p.xy[0], p.xy[1], p.t % len(valleys)}
	}

	neighbors := func(p path) []path {
		n := []path{}
		for _, xy := range []XY{
			p.xy,
			AdvanceXY(p.xy, DirR, 1),
			AdvanceXY(p.xy, DirD, 1),
			AdvanceXY(p.xy, DirL, 1),
			AdvanceXY(p.xy, DirU, 1),
		} {
			if xy[0] < 0 || xy[0] > goal[0] || xy[1] < 0 || xy[1] > goal[1] || valleys[(p.t+1)%len(valleys)][xy[1]].Contains(xy[0]) {
				continue
			}
			n = append(n, path{xy: xy, t: p.t + 1})
		}
		return n
	}

	p, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.t)
}

func (aoc *aoc202224) Part2(input *Input) string {
	goal, valleys := aoc.parse(input)

	type path struct {
		xy    XY
		stage int
		t     int
	}

	start := []path{path{xy: XY{1, 0}}}

	done := func(p path) bool {
		return p.stage == 2 && p.xy == goal
	}

	snax := XY{1, 0}
	priority := func(p path) int {
		return -p.t - (goal[0] - p.xy[0] + goal[1] - p.xy[1]) - 1000*(2-p.stage)
	}

	state := func(p path) [3]int {
		return [3]int{p.xy[0], p.xy[1], p.t % len(valleys)}
	}

	neighbors := func(p path) []path {
		n := []path{}
		for _, xy := range []XY{
			p.xy,
			AdvanceXY(p.xy, DirR, 1),
			AdvanceXY(p.xy, DirD, 1),
			AdvanceXY(p.xy, DirL, 1),
			AdvanceXY(p.xy, DirU, 1),
		} {
			if xy[0] < 0 || xy[0] > goal[0] || xy[1] < 0 || xy[1] > goal[1] || valleys[(p.t+1)%len(valleys)][xy[1]].Contains(xy[0]) {
				continue
			}
			stage := p.stage
			if (xy == goal && stage == 0) || (xy == snax && stage == 1) {
				stage++
			}
			n = append(n, path{xy: xy, stage: stage, t: p.t + 1})
		}
		return n
	}

	p, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.t)
}
