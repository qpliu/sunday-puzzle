package main

func init() {
	Register(&aoc202109{
		AOC: AOC{
			Day:           9,
			InputFilename: "../2021/input/09.txt",
			Tests: []Test{
				Test{
					Input: `2199943210
3987894921
9856789892
8767896789
9899965678
`,
					Part1: "15",
					Part2: "1134",
				},
			},
		},
	})
}

type aoc202109 struct {
	AOC
}

func (aoc *aoc202109) Part1(input *Input) string {
	_, _, grid := input.Grid()
	result := 0
	for xy, h := range grid {
		n := grid[XY{xy[0] - 1, xy[1]}]
		if n != 0 && n <= h {
			continue
		}
		n = grid[XY{xy[0] + 1, xy[1]}]
		if n != 0 && n <= h {
			continue
		}
		n = grid[XY{xy[0], xy[1] - 1}]
		if n != 0 && n <= h {
			continue
		}
		n = grid[XY{xy[0], xy[1] + 1}]
		if n != 0 && n <= h {
			continue
		}
		result += int(h + 1 - '0')
	}
	return IntResult(result)
}

func (aoc *aoc202109) Part2(input *Input) string {
	_, _, grid := input.Grid()
	size1, size2, size3 := 0, 0, 0
	visited := map[XY]bool{}
	queue := NewQueue[XY]()
	for xy, h := range grid {
		if h == '9' || visited[xy] {
			continue
		}
		size := 0
		queue.Enqueue(xy)
		for !queue.Empty() {
			fillXY := queue.Dequeue()
			if visited[fillXY] || grid[fillXY] == '9' || grid[fillXY] == 0 {
				continue
			}
			size++
			visited[fillXY] = true
			queue.Enqueue(XY{fillXY[0] + 1, fillXY[1]})
			queue.Enqueue(XY{fillXY[0] - 1, fillXY[1]})
			queue.Enqueue(XY{fillXY[0], fillXY[1] + 1})
			queue.Enqueue(XY{fillXY[0], fillXY[1] - 1})
		}
		if size > size1 {
			size1, size = size, size1
		}
		if size > size2 {
			size2, size = size, size2
		}
		if size > size3 {
			size3, size = size, size3
		}
	}
	return IntResult(size1 * size2 * size2)
}
