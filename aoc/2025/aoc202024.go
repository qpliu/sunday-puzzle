package main

func init() {
	Register(&aoc202024{
		AOC: AOC{
			Day:           24,
			InputFilename: "../2020/input/24.txt",
			Tests: []Test{
				Test{
					Input: `sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew
`,
					Part1: "10",
					Part2: "2208",
				},
			},
		},
	})
}

type aoc202024 struct {
	AOC
}

func (aoc *aoc202024) parse(input *Input) map[XY]uint8 {
	tiles := map[XY]uint8{}
	ns := 0
	xy := XY{}
	for ch := range input.Chars() {
		switch ch {
		case '\n':
			if tiles[xy] == 1 {
				delete(tiles, xy)
			} else {
				tiles[xy] = 1
			}
			if ns != 0 {
				panic("bad input")
			}
			xy = XY{}
		case 'n':
			if ns != 0 {
				panic("bad input")
			}
			ns = -1
		case 's':
			if ns != 0 {
				panic("bad input")
			}
			ns = 1
		case 'e':
			switch ns {
			case -1:
				xy[0] += xy[1] & 1
				xy[1]--
				ns = 0
			case 0:
				xy[0]++
			case 1:
				xy[0] += xy[1] & 1
				xy[1]++
				ns = 0
			default:
				panic("?")
			}
		case 'w':
			switch ns {
			case -1:
				xy[0] -= 1 - xy[1]&1
				xy[1]--
				ns = 0
			case 0:
				xy[0]--
			case 1:
				xy[0] -= 1 - xy[1]&1
				xy[1]++
				ns = 0
			default:
				panic("?")
			}
		default:
			panic("bad input")
		}
	}
	return tiles
}

func (aoc *aoc202024) Part1(input *Input) string {
	return IntResult(len(aoc.parse(input)))
}

func (aoc *aoc202024) Part2(input *Input) string {
	tiles := aoc.parse(input)
	adjacent := map[XY]bool{}
	next := map[XY]uint8{}
	for range 100 {
		clear(adjacent)
		for xy := range tiles {
			dx := -1 + 2*(xy[1]&1)
			adjacent[xy] = true
			adjacent[XY{xy[0] + 1, xy[1]}] = true
			adjacent[XY{xy[0] - 1, xy[1]}] = true
			adjacent[XY{xy[0], xy[1] + 1}] = true
			adjacent[XY{xy[0], xy[1] - 1}] = true
			adjacent[XY{xy[0] + dx, xy[1] + 1}] = true
			adjacent[XY{xy[0] + dx, xy[1] - 1}] = true
		}
		clear(next)
		for xy := range adjacent {
			dx := -1 + 2*(xy[1]&1)
			n := tiles[XY{xy[0] + 1, xy[1]}] + tiles[XY{xy[0] - 1, xy[1]}] + tiles[XY{xy[0], xy[1] + 1}] + tiles[XY{xy[0], xy[1] - 1}] + tiles[XY{xy[0] + dx, xy[1] + 1}] + tiles[XY{xy[0] + dx, xy[1] - 1}]
			if n == 2 || (n == 1 && tiles[xy] == 1) {
				next[xy] = 1
			}
		}
		tiles, next = next, tiles
	}
	return IntResult(len(tiles))
}
