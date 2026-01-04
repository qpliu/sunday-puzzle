package main

func init() {
	Register(&aoc202217{
		AOC: AOC{
			Day:           17,
			InputFilename: "../2022/input/17.txt",
			Tests: []Test{
				Test{
					Input: `>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
`,
					Part1: "3068",
					Part2: "1514285714288",
				},
			},
		},
	})
}

type aoc202217 struct {
	AOC
}

func (aoc *aoc202217) rocks() [5][][]byte {
	return [5][][]byte{
		// ####
		[][]byte{
			[]byte{0xf},
			[]byte{0x1e},
			[]byte{0x3c},
			[]byte{0x78},
		},
		// .#.
		// ###
		// .#.
		[][]byte{
			[]byte{0x2, 0x7, 0x2},
			[]byte{0x4, 0xe, 0x4},
			[]byte{0x8, 0x1c, 0x8},
			[]byte{0x10, 0x38, 0x10},
			[]byte{0x20, 0x70, 0x20},
		},
		// ..#
		// ..#
		// ###
		[][]byte{
			[]byte{0x7, 0x4, 0x4},
			[]byte{0xe, 0x8, 0x8},
			[]byte{0x1c, 0x10, 0x10},
			[]byte{0x38, 0x20, 0x20},
			[]byte{0x70, 0x40, 0x40},
		},
		// #
		// #
		// #
		// #
		[][]byte{
			[]byte{0x1, 0x1, 0x1, 0x1},
			[]byte{0x2, 0x2, 0x2, 0x2},
			[]byte{0x4, 0x4, 0x4, 0x4},
			[]byte{0x8, 0x8, 0x8, 0x8},
			[]byte{0x10, 0x10, 0x10, 0x10},
			[]byte{0x20, 0x20, 0x20, 0x20},
			[]byte{0x40, 0x40, 0x40, 0x40},
		},
		// ##
		// ##
		[][]byte{
			[]byte{0x3, 0x3},
			[]byte{0x6, 0x6},
			[]byte{0xc, 0xc},
			[]byte{0x18, 0x18},
			[]byte{0x30, 0x30},
			[]byte{0x60, 0x60},
		},
	}
}

func (aoc *aoc202217) parse(input *Input) []int {
	jets := []int{}
	for ch := range input.Chars() {
		switch ch {
		case '<':
			jets = append(jets, -1)
		case '>':
			jets = append(jets, 1)
		}
	}
	return jets
}

func (aoc *aoc202217) drop(rock [][]byte, jets []int, t int, tower []byte) (int, []byte) {
	x := 2
	x = min(max(0, x+jets[t%len(jets)]), len(rock)-1)
	t++
	x = min(max(0, x+jets[t%len(jets)]), len(rock)-1)
	t++
	x = min(max(0, x+jets[t%len(jets)]), len(rock)-1)
	t++
	y := len(tower)
	for {
		xnext := x + jets[t%len(jets)]
		t++
		if xnext >= 0 && xnext < len(rock) {
			for dy, r := range rock[xnext] {
				if y+dy < len(tower) && r&tower[y+dy] != 0 {
					xnext = x
					break
				}
			}
			x = xnext
		}
		for dy, r := range rock[x] {
			if y+dy-1 < len(tower) && r&tower[y+dy-1] != 0 {
				for dy, r := range rock[x] {
					if y+dy < len(tower) {
						tower[y+dy] |= r
					} else {
						tower = append(tower, r)
					}
				}
				return t, tower
			}
		}
		y--
	}
}

func (aoc *aoc202217) Part1(input *Input) string {
	rocks := aoc.rocks()
	jets := aoc.parse(input)
	tower := []byte{0x7f}
	t := 0
	for rock := range 2022 {
		t, tower = aoc.drop(rocks[rock%len(rocks)], jets, t, tower)
	}
	return IntResult(len(tower) - 1)
}

func (aoc *aoc202217) rebase(base int, tower []byte) (int, []byte) {
	reachable := byte(0x7f)
	for y := len(tower) - 1; y >= 0; y-- {
		reachable = ((reachable | reachable<<1 | reachable>>1) & 0x7f) &^ tower[y]
		if reachable == 0 {
			return base + y, tower[y:]
		}
	}
	return base, tower
}

func (aoc *aoc202217) Part2(input *Input) string {
	rocks := aoc.rocks()
	jets := aoc.parse(input)
	tower := []byte{0x7f}
	base := -1
	t := 0
	states := map[string][2]int{}
	for rock := range len(rocks) * len(jets) {
		if t >= len(jets) && rock%len(rocks) == 0 {
			t %= len(jets)
			base, tower = aoc.rebase(base, tower)
			state := make([]byte, 4+len(tower))
			state[0] = byte(t & 127)
			state[1] = byte((t >> 7) & 127)
			state[2] = byte((t >> 14) & 127)
			state[3] = byte((t >> 21) & 127)
			copy(state[4:], tower)
			if prev, ok := states[string(state)]; !ok {
				states[string(state)] = [2]int{base, rock}
			} else {
				ncycles := (1000000000000 - rock) / (rock - prev[1])
				base += ncycles * (base - prev[0])
				for dr := range (1000000000000 - rock) % (rock - prev[1]) {
					t, tower = aoc.drop(rocks[dr%len(rocks)], jets, t, tower)
				}
				return IntResult(base + len(tower))
			}
		}
		t, tower = aoc.drop(rocks[rock%len(rocks)], jets, t, tower)
	}
	panic("bad input")
}
