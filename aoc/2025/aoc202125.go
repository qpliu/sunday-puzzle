package main

func init() {
	Register(&aoc202125{
		AOC: AOC{
			Day:           25,
			InputFilename: "../2021/input/25.txt",
			Tests: []Test{
				Test{
					Input: `v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>
`,
					Part1: "58",
					Part2: "",
				},
			},
		},
	})
}

type aoc202125 struct {
	AOC
}

func (aoc *aoc202125) Part1(input *Input) string {
	w, h := 0, 0
	east, south := map[XY]bool{}, map[XY]bool{}
	{
		x, y := 0, 0
		for ch := range input.Chars() {
			switch ch {
			case '\n':
				x = 0
				y++
			case '>':
				east[XY{x, y}] = true
				w = max(w, x+1)
				h = max(h, y+1)
				x++
			case 'v':
				south[XY{x, y}] = true
				w = max(w, x+1)
				h = max(h, y+1)
				x++
			default:
				w = max(w, x+1)
				h = max(h, y+1)
				x++
			}
		}
	}
	tmp := map[XY]bool{}
	steps := 0
	moved := true
	for moved {
		steps++
		moved = false
		clear(tmp)
		for xy := range east {
			dest := XY{(xy[0] + 1) % w, xy[1]}
			if east[dest] || south[dest] {
				tmp[xy] = true
			} else {
				moved = true
				tmp[dest] = true
			}
		}
		east, tmp = tmp, east
		clear(tmp)
		for xy := range south {
			dest := XY{xy[0], (xy[1] + 1) % h}
			if east[dest] || south[dest] {
				tmp[xy] = true
			} else {
				moved = true
				tmp[dest] = true
			}
		}
		south, tmp = tmp, south
	}
	return IntResult(steps)
}

func (aoc *aoc202125) Part2(input *Input) string {
	return ""
}
