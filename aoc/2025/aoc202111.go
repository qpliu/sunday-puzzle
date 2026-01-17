package main

func init() {
	Register(&aoc202111{
		AOC: AOC{
			Day:           11,
			InputFilename: "../2021/input/11.txt",
			Tests: []Test{
				Test{
					Input: `5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526
`,
					Part1: "1656",
					Part2: "195",
				},
			},
		},
	})
}

type aoc202111 struct {
	AOC
}

func (aoc *aoc202111) Part1(input *Input) string {
	_, _, grid := input.Grid()
	flashed := map[XY]bool{}
	result := 0
	queue := NewQueue[XY]()
	enqueue := func(xy XY) {
		b := grid[xy]
		if b != 0 && !flashed[xy] {
			queue.Enqueue(xy)
		}
	}
	for range 100 {
		clear(flashed)
		for xy, b := range grid {
			if b == '9' {
				result++
				flashed[xy] = true
				enqueue(XY{xy[0] + 1, xy[1]})
				enqueue(XY{xy[0] + 1, xy[1] + 1})
				enqueue(XY{xy[0], xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1]})
				enqueue(XY{xy[0] - 1, xy[1] - 1})
				enqueue(XY{xy[0], xy[1] - 1})
				enqueue(XY{xy[0] + 1, xy[1] - 1})
			} else {
				grid[xy] = b + 1
			}
		}
		for !queue.Empty() {
			xy := queue.Dequeue()
			if flashed[xy] {
				continue
			}
			b := grid[xy]
			if b == '9' {
				result++
				flashed[xy] = true
				enqueue(XY{xy[0] + 1, xy[1]})
				enqueue(XY{xy[0] + 1, xy[1] + 1})
				enqueue(XY{xy[0], xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1]})
				enqueue(XY{xy[0] - 1, xy[1] - 1})
				enqueue(XY{xy[0], xy[1] - 1})
				enqueue(XY{xy[0] + 1, xy[1] - 1})
			} else {
				grid[xy] = b + 1
			}
		}
		for xy := range flashed {
			grid[xy] = '0'
		}
	}
	return IntResult(result)
}

func (aoc *aoc202111) Part2(input *Input) string {
	_, _, grid := input.Grid()
	flashed := map[XY]bool{}
	queue := NewQueue[XY]()
	enqueue := func(xy XY) {
		b := grid[xy]
		if b != 0 && !flashed[xy] {
			queue.Enqueue(xy)
		}
	}
	step := 1
	for {
		clear(flashed)
		for xy, b := range grid {
			if b == '9' {
				flashed[xy] = true
				enqueue(XY{xy[0] + 1, xy[1]})
				enqueue(XY{xy[0] + 1, xy[1] + 1})
				enqueue(XY{xy[0], xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1]})
				enqueue(XY{xy[0] - 1, xy[1] - 1})
				enqueue(XY{xy[0], xy[1] - 1})
				enqueue(XY{xy[0] + 1, xy[1] - 1})
			} else {
				grid[xy] = b + 1
			}
		}
		for !queue.Empty() {
			xy := queue.Dequeue()
			if flashed[xy] {
				continue
			}
			b := grid[xy]
			if b == '9' {
				flashed[xy] = true
				enqueue(XY{xy[0] + 1, xy[1]})
				enqueue(XY{xy[0] + 1, xy[1] + 1})
				enqueue(XY{xy[0], xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1] + 1})
				enqueue(XY{xy[0] - 1, xy[1]})
				enqueue(XY{xy[0] - 1, xy[1] - 1})
				enqueue(XY{xy[0], xy[1] - 1})
				enqueue(XY{xy[0] + 1, xy[1] - 1})
			} else {
				grid[xy] = b + 1
			}
		}
		if len(flashed) == len(grid) {
			return IntResult(step)
		}
		step++
		for xy := range flashed {
			grid[xy] = '0'
		}
	}
}
