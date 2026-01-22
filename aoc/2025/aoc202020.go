package main

import (
	"math/bits"
	"runtime"
)

func init() {
	Register(&aoc202020{
		AOC: AOC{
			Day:           20,
			InputFilename: "../2020/input/20.txt",
			Tests: []Test{
				Test{
					Input: `Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...
`,
					Part1: "20899048083289",
					Part2: "273",
				},
			},
		},
	})
}

type aoc202020 struct {
	AOC
}

// tile is {id,top border,left border,right border,bottom border,tile lines 0-7}
func (aoc *aoc202020) parse(input *Input) [][13]int {
	tiles := [][13]int{}
	for {
		tile := [13]int{}
		tileID, ok := input.Int()
		if !ok {
			return tiles
		}
		tile[0] = tileID
		input.Skip(":\n")
		if ch, _ := input.Char(); ch == '#' {
			tile[1] |= 1
			tile[2] |= 1
		}
		for j := range 8 {
			if ch, _ := input.Char(); ch == '#' {
				tile[1] |= 2 << j
			}
		}
		if ch, _ := input.Char(); ch == '#' {
			tile[1] |= 512
			tile[3] |= 1
		}
		input.Skip("\n")
		for k := range 8 {
			if ch, _ := input.Char(); ch == '#' {
				tile[2] |= 2 << k
			}
			for j := range 8 {
				if ch, _ := input.Char(); ch == '#' {
					tile[5+k] |= 1 << j
				}
			}
			if ch, _ := input.Char(); ch == '#' {
				tile[3] |= 2 << k
			}
			input.Skip("\n")
		}
		if ch, _ := input.Char(); ch == '#' {
			tile[2] |= 512
			tile[4] |= 1
		}
		for j := range 8 {
			if ch, _ := input.Char(); ch == '#' {
				tile[4] |= 2 << j
			}
		}
		if ch, _ := input.Char(); ch == '#' {
			tile[3] |= 512
			tile[4] |= 512
		}
		tiles = append(tiles, tile)
	}
}

func (aoc *aoc202020) flip(n, nbits int) int {
	flipped := 0
	for i := range nbits {
		if n&(1<<i) != 0 {
			flipped |= 1 << (nbits - 1 - i)
		}
	}
	return flipped
}

func (aoc *aoc202020) Part1(input *Input) string {
	tiles := aoc.parse(input)
	borders := map[int]int{}
	for _, tile := range tiles {
		for j := range 4 {
			borders[tile[1+j]]++
			borders[aoc.flip(tile[1+j], 10)]++
		}
	}
	result := 1
	for _, tile := range tiles {
		count := 0
		for j := range 4 {
			count += borders[tile[1+j]]
		}
		if count == 6 {
			result *= tile[0]
		}
	}
	return IntResult(result)
}

func (aoc *aoc202020) vflip(tile [13]int) [13]int {
	return [13]int{
		tile[0],
		tile[4],
		aoc.flip(tile[2], 10),
		aoc.flip(tile[3], 10),
		tile[1],
		tile[12],
		tile[11],
		tile[10],
		tile[9],
		tile[8],
		tile[7],
		tile[6],
		tile[5],
	}
}

func (aoc *aoc202020) hflip(tile [13]int) [13]int {
	return [13]int{
		tile[0],
		aoc.flip(tile[1], 10),
		tile[3],
		tile[2],
		aoc.flip(tile[4], 10),
		aoc.flip(tile[5], 8),
		aoc.flip(tile[6], 8),
		aoc.flip(tile[7], 8),
		aoc.flip(tile[8], 8),
		aoc.flip(tile[9], 8),
		aoc.flip(tile[10], 8),
		aoc.flip(tile[11], 8),
		aoc.flip(tile[12], 8),
	}
}

func (aoc *aoc202020) rotateTile(tile [13]int) [13]int {
	return [13]int{
		tile[0],
		aoc.flip(tile[2], 10),
		tile[4],
		tile[1],
		aoc.flip(tile[3], 10),
		(tile[12]&1)<<0 | (tile[11] & 1 << 1) | (tile[10] & 1 << 2) | (tile[9] & 1 << 3) | (tile[8] & 1 << 4) | (tile[7] & 1 << 5) | (tile[6] & 1 << 6) | (tile[5] & 1 << 7),
		(tile[12]&2)>>1 | (tile[11] & 2 << 0) | (tile[10] & 2 << 1) | (tile[9] & 2 << 2) | (tile[8] & 2 << 3) | (tile[7] & 2 << 4) | (tile[6] & 2 << 5) | (tile[5] & 2 << 6),
		(tile[12]&4)>>2 | (tile[11] & 4 >> 1) | (tile[10] & 4 << 0) | (tile[9] & 4 << 1) | (tile[8] & 4 << 2) | (tile[7] & 4 << 3) | (tile[6] & 4 << 4) | (tile[5] & 4 << 5),
		(tile[12]&8)>>3 | (tile[11] & 8 >> 2) | (tile[10] & 8 >> 1) | (tile[9] & 8 << 0) | (tile[8] & 8 << 1) | (tile[7] & 8 << 2) | (tile[6] & 8 << 3) | (tile[5] & 8 << 4),
		(tile[12]&16)>>4 | (tile[11] & 16 >> 3) | (tile[10] & 16 >> 2) | (tile[9] & 16 >> 1) | (tile[8] & 16 << 0) | (tile[7] & 16 << 1) | (tile[6] & 16 << 2) | (tile[5] & 16 << 3),
		(tile[12]&32)>>5 | (tile[11] & 32 >> 4) | (tile[10] & 32 >> 3) | (tile[9] & 32 >> 2) | (tile[8] & 32 >> 1) | (tile[7] & 32 << 0) | (tile[6] & 32 << 1) | (tile[5] & 32 << 2),
		(tile[12]&64)>>6 | (tile[11] & 64 >> 5) | (tile[10] & 64 >> 4) | (tile[9] & 64 >> 3) | (tile[8] & 64 >> 2) | (tile[7] & 64 >> 1) | (tile[6] & 64 << 0) | (tile[5] & 64 << 1),
		(tile[12]&128)>>7 | (tile[11] & 128 >> 6) | (tile[10] & 128 >> 5) | (tile[9] & 128 >> 4) | (tile[8] & 128 >> 3) | (tile[7] & 128 >> 2) | (tile[6] & 128 >> 1) | (tile[5] & 128 << 0),
	}
}

func (aoc *aoc202020) construct(input *Input) [][][13]int {
	tiles := aoc.parse(input)
	borders := map[int][]int{}
	for i, tile := range tiles {
		for j := range 4 {
			borders[tile[1+j]] = append(borders[tile[1+j]], i)
			borders[aoc.flip(tile[1+j], 10)] = append(borders[aoc.flip(tile[1+j], 10)], i)
		}
	}
	n := 1
	for n*n < len(tiles) {
		n++
	}
	grid := make([][][13]int, n)
	for i := range n {
		grid[i] = make([][13]int, n)
	}
	for _, tile := range tiles {
		up := len(borders[tile[1]])
		lt := len(borders[tile[2]])
		rt := len(borders[tile[3]])
		dn := len(borders[tile[4]])
		if up+lt+rt+dn != 6 {
			continue
		}
		if up == 1 && lt == 1 {
			grid[0][0] = tile
		} else if up == 1 && rt == 1 {
			grid[0][0] = aoc.hflip(tile)
		} else if dn == 1 && lt == 1 {
			grid[0][0] = aoc.vflip(tile)
		} else if dn == 1 && rt == 1 {
			grid[0][0] = aoc.hflip(aoc.vflip(tile))
		} else {
			panic("bad input")
		}
		break
	}
	matchRight := func(t [13]int) [13]int {
		b := t[3]
		tile := [13]int{}
		for _, i := range borders[b] {
			if tiles[i][0] != t[0] {
				tile = tiles[i]
				break
			}
		}
		if tile[2] == b {
			return tile
		} else if aoc.flip(tile[2], 10) == b {
			return aoc.vflip(tile)
		} else if tile[3] == b {
			return aoc.hflip(tile)
		} else if aoc.flip(tile[3], 10) == b {
			return aoc.vflip(aoc.hflip(tile))
		} else if tile[4] == b {
			return aoc.rotateTile(tile)
		} else if aoc.flip(tile[4], 10) == b {
			return aoc.vflip(aoc.rotateTile(tile))
		} else if tile[1] == b {
			return aoc.hflip(aoc.rotateTile(tile))
		} else if aoc.flip(tile[1], 10) == b {
			return aoc.vflip(aoc.hflip(aoc.rotateTile(tile)))
		} else {
			panic("bad input")
		}
	}
	for i := range n - 1 {
		grid[i+1][0] = matchRight(grid[i][0])
	}
	matchDown := func(t [13]int) [13]int {
		b := t[4]
		tile := [13]int{}
		for _, i := range borders[b] {
			if tiles[i][0] != t[0] {
				tile = tiles[i]
				break
			}
		}
		if tile[1] == b {
			return tile
		} else if aoc.flip(tile[1], 10) == b {
			return aoc.hflip(tile)
		} else if tile[4] == b {
			return aoc.vflip(tile)
		} else if aoc.flip(tile[4], 10) == b {
			return aoc.vflip(aoc.hflip(tile))
		} else if tile[2] == b {
			return aoc.hflip(aoc.rotateTile(tile))
		} else if aoc.flip(tile[2], 10) == b {
			return aoc.rotateTile(tile)
		} else if tile[3] == b {
			return aoc.vflip(aoc.hflip(aoc.rotateTile(tile)))
		} else if aoc.flip(tile[3], 10) == b {
			return aoc.vflip(aoc.rotateTile(tile))
		} else {
			panic("bad input")
		}
	}
	for j := range n - 1 {
		for i := range n {
			grid[i][j+1] = matchDown(grid[i][j])
		}
	}
	return grid
}

func (aoc *aoc202020) rotateGrid(grid [][][13]int) [][][13]int {
	n := len(grid)
	rgrid := make([][][13]int, n)
	for i := range n {
		rgrid[i] = make([][13]int, n)
		for j := range n {
			rgrid[i][j] = aoc.rotateTile(grid[j][n-1-i])
		}
	}
	return rgrid
}

func (aoc *aoc202020) monsters() ([][][3]int, [][][3]int) {
	// ........ ........ ..#.
	// #....##. ...##... .###
	// .#..#..# ..#..#.. #...
	monster := []int{0x40000, 0xe1861, 0x12492}
	fmonster := []int{
		aoc.flip(monster[0], 20),
		aoc.flip(monster[1], 20),
		aoc.flip(monster[2], 20),
	}
	monsters := [][][3]int{}
	fmonsters := [][][3]int{}
	for i := range 8 {
		m := [][3]int{}
		fm := [][3]int{}
		for j := range 4 {
			if (monster[1]<<8)>>((j+1)*8-i) == 0 {
				break
			}
			m = append(m, [3]int{
				((monster[0] << 8) >> ((j+1)*8 - i)) & 255,
				((monster[1] << 8) >> ((j+1)*8 - i)) & 255,
				((monster[2] << 8) >> ((j+1)*8 - i)) & 255,
			})
			fm = append(fm, [3]int{
				((fmonster[0] << 8) >> ((j+1)*8 - i)) & 255,
				((fmonster[1] << 8) >> ((j+1)*8 - i)) & 255,
				((fmonster[2] << 8) >> ((j+1)*8 - i)) & 255,
			})
		}
		monsters = append(monsters, m)
		fmonsters = append(fmonsters, fm)
	}
	return monsters, fmonsters
}

func (aoc *aoc202020) Part2(input *Input) string {
	grid := aoc.construct(input)
	rgrid := aoc.rotateGrid(grid)
	n := len(grid)
	monsters, fmonsters := aoc.monsters()

	in := make(chan int)
	out := make(chan [8]int)
	for range runtime.NumCPU() {
		go func() {
			monsterCounts := [8]int{}
			for i := range in {
				y0 := i / 8
				r0 := 5 + i%8
				y1 := (i + 1) / 8
				r1 := 5 + (i+1)%8
				y2 := (i + 2) / 8
				r2 := 5 + (i+2)%8
				for x := range n {
					for m := range monsters {
						if x+len(monsters[m]) > n {
							continue
						}
						nm := 1
						nudm := 1 // upside-down
						nfm := 1  // flipped
						nudfm := 1
						nrm := 1 // rotated
						nrudm := 1
						nrfm := 1
						nrudfm := 1
						for dx := range monsters[m] {
							if grid[x+dx][y0][r0]&monsters[m][dx][0] != monsters[m][dx][0] || grid[x+dx][y1][r1]&monsters[m][dx][1] != monsters[m][dx][1] || grid[x+dx][y2][r2]&monsters[m][dx][2] != monsters[m][dx][2] {
								nm = 0
							}
							if grid[x+dx][y0][r0]&monsters[m][dx][2] != monsters[m][dx][2] || grid[x+dx][y1][r1]&monsters[m][dx][1] != monsters[m][dx][1] || grid[x+dx][y2][r2]&monsters[m][dx][0] != monsters[m][dx][0] {
								nudm = 0
							}
							if grid[x+dx][y0][r0]&fmonsters[m][dx][0] != fmonsters[m][dx][0] || grid[x+dx][y1][r1]&fmonsters[m][dx][1] != fmonsters[m][dx][1] || grid[x+dx][y2][r2]&fmonsters[m][dx][2] != fmonsters[m][dx][2] {
								nfm = 0
							}
							if grid[x+dx][y0][r0]&fmonsters[m][dx][2] != fmonsters[m][dx][2] || grid[x+dx][y1][r1]&fmonsters[m][dx][1] != fmonsters[m][dx][1] || grid[x+dx][y2][r2]&fmonsters[m][dx][0] != fmonsters[m][dx][0] {
								nudfm = 0
							}
							if rgrid[x+dx][y0][r0]&monsters[m][dx][0] != monsters[m][dx][0] || rgrid[x+dx][y1][r1]&monsters[m][dx][1] != monsters[m][dx][1] || rgrid[x+dx][y2][r2]&monsters[m][dx][2] != monsters[m][dx][2] {
								nrm = 0
							}
							if rgrid[x+dx][y0][r0]&monsters[m][dx][2] != monsters[m][dx][2] || rgrid[x+dx][y1][r1]&monsters[m][dx][1] != monsters[m][dx][1] || rgrid[x+dx][y2][r2]&monsters[m][dx][0] != monsters[m][dx][0] {
								nrudm = 0
							}
							if rgrid[x+dx][y0][r0]&fmonsters[m][dx][0] != fmonsters[m][dx][0] || rgrid[x+dx][y1][r1]&fmonsters[m][dx][1] != fmonsters[m][dx][1] || rgrid[x+dx][y2][r2]&fmonsters[m][dx][2] != fmonsters[m][dx][2] {
								nrfm = 0
							}
							if rgrid[x+dx][y0][r0]&fmonsters[m][dx][2] != fmonsters[m][dx][2] || rgrid[x+dx][y1][r1]&fmonsters[m][dx][1] != fmonsters[m][dx][1] || rgrid[x+dx][y2][r2]&fmonsters[m][dx][0] != fmonsters[m][dx][0] {
								nrudfm = 0
							}
						}
						monsterCounts[0] += nm
						monsterCounts[1] += nudm
						monsterCounts[2] += nfm
						monsterCounts[3] += nudfm
						monsterCounts[4] += nrm
						monsterCounts[5] += nrudm
						monsterCounts[6] += nrfm
						monsterCounts[7] += nrudfm
					}
				}
			}
			out <- monsterCounts
		}()
	}
	for i := range 8*n - 2 {
		in <- i
	}
	close(in)
	monsterCounts := [8]int{}
	for range runtime.NumCPU() {
		counts := <-out
		for i := range counts {
			monsterCounts[i] += counts[i]
		}
	}

	nmonsters := 0
	for _, count := range monsterCounts {
		nmonsters = max(nmonsters, count)
	}
	total := 0
	for i := range n {
		for j := range n {
			for k := range 8 {
				total += bits.OnesCount(uint(grid[i][j][5+k]))
			}
		}
	}
	return IntResult(total - 15*nmonsters)
}
