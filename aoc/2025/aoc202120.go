package main

func init() {
	Register(&aoc202120{
		AOC: AOC{
			Day:           20,
			InputFilename: "../2021/input/20.txt",
			Tests: []Test{
				Test{
					Input: `..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###
`,
					Part1: "35",
					Part2: "3351",
				},
			},
		},
	})
}

type aoc202120 struct {
	AOC
}

func (aoc *aoc202120) parse(input *Input) (BitSet512, int, int, map[XY]bool) {
	algo := BitSet512{}
	i := 0
	for ch := range input.Chars() {
		switch ch {
		case '#':
			algo = algo.Add(i)
			fallthrough
		case '.':
			i++
		}
		if i >= 512 {
			break
		}
	}
	input.Char()
	image := map[XY]bool{}
	xmax, ymax := 0, 0
	x, y := 0, 0
	for ch := range input.Chars() {
		switch ch {
		case '\n':
			x = 0
			y++
		case '#':
			image[XY{x, y}] = true
			xmax = max(xmax, x)
			ymax = y
			fallthrough
		default:
			x++
		}
	}
	return algo, xmax, ymax, image
}

func (aoc *aoc202120) result(niters int, input *Input) string {
	algo, xmax, ymax, image := aoc.parse(input)
	if algo.Contains(0) && algo.Contains(511) {
		panic("bad input")
	}
	tmp := map[XY]bool{}
	if algo.Contains(0) {
		for range niters {
			clear(tmp)
			for x := range xmax + 3 {
				for y := range ymax + 3 {
					index := 0
					if image[XY{x - 2, y - 2}] {
						index |= 256
					}
					if image[XY{x - 1, y - 2}] {
						index |= 128
					}
					if image[XY{x, y - 2}] {
						index |= 64
					}
					if image[XY{x - 2, y - 1}] {
						index |= 32
					}
					if image[XY{x - 1, y - 1}] {
						index |= 16
					}
					if image[XY{x, y - 1}] {
						index |= 8
					}
					if image[XY{x - 2, y}] {
						index |= 4
					}
					if image[XY{x - 1, y}] {
						index |= 2
					}
					if image[XY{x, y}] {
						index |= 1
					}
					if !algo.Contains(index) {
						tmp[XY{x, y}] = true
					}
				}
			}
			xmax += 2
			ymax += 2
			clear(image)
			for x := range xmax + 3 {
				for y := range ymax + 3 {
					index := 0
					if !tmp[XY{x - 2, y - 2}] {
						index |= 256
					}
					if !tmp[XY{x - 1, y - 2}] {
						index |= 128
					}
					if !tmp[XY{x, y - 2}] {
						index |= 64
					}
					if !tmp[XY{x - 2, y - 1}] {
						index |= 32
					}
					if !tmp[XY{x - 1, y - 1}] {
						index |= 16
					}
					if !tmp[XY{x, y - 1}] {
						index |= 8
					}
					if !tmp[XY{x - 2, y}] {
						index |= 4
					}
					if !tmp[XY{x - 1, y}] {
						index |= 2
					}
					if !tmp[XY{x, y}] {
						index |= 1
					}
					if algo.Contains(index) {
						image[XY{x, y}] = true
					}
				}
			}
			xmax += 2
			ymax += 2
		}
	} else {
		for range niters {
			clear(tmp)
			for x := range xmax + 3 {
				for y := range ymax + 3 {
					index := 0
					if image[XY{x - 2, y - 2}] {
						index |= 256
					}
					if image[XY{x - 1, y - 2}] {
						index |= 128
					}
					if image[XY{x, y - 2}] {
						index |= 64
					}
					if image[XY{x - 2, y - 1}] {
						index |= 32
					}
					if image[XY{x - 1, y - 1}] {
						index |= 16
					}
					if image[XY{x, y - 1}] {
						index |= 8
					}
					if image[XY{x - 2, y}] {
						index |= 4
					}
					if image[XY{x - 1, y}] {
						index |= 2
					}
					if image[XY{x, y}] {
						index |= 1
					}
					if algo.Contains(index) {
						tmp[XY{x, y}] = true
					}
				}
			}
			xmax += 2
			ymax += 2
			clear(image)
			for x := range xmax + 3 {
				for y := range ymax + 3 {
					index := 0
					if tmp[XY{x - 2, y - 2}] {
						index |= 256
					}
					if tmp[XY{x - 1, y - 2}] {
						index |= 128
					}
					if tmp[XY{x, y - 2}] {
						index |= 64
					}
					if tmp[XY{x - 2, y - 1}] {
						index |= 32
					}
					if tmp[XY{x - 1, y - 1}] {
						index |= 16
					}
					if tmp[XY{x, y - 1}] {
						index |= 8
					}
					if tmp[XY{x - 2, y}] {
						index |= 4
					}
					if tmp[XY{x - 1, y}] {
						index |= 2
					}
					if tmp[XY{x, y}] {
						index |= 1
					}
					if algo.Contains(index) {
						image[XY{x, y}] = true
					}
				}
			}
			xmax += 2
			ymax += 2
		}
	}
	return IntResult(len(image))
}

func (aoc *aoc202120) Part1(input *Input) string {
	return aoc.result(1, input)
}

func (aoc *aoc202120) Part2(input *Input) string {
	return aoc.result(25, input)
}
