package main

import (
	"runtime"
	"slices"
)

func init() {
	Register(&aoc202322{
		AOC: AOC{
			Day:           22,
			InputFilename: "../2023/input/22.txt",
			Tests: []Test{
				Test{
					Input: `1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
`,
					Part1: "5",
					Part2: "7",
				},
			},
		},
	})
}

type aoc202322 struct {
	AOC
}

func (aoc *aoc202322) parse(input *Input) [][2]map[int]bool {
	bricks := [][6]int{}
	for {
		x1, _ := input.Int()
		y1, _ := input.Int()
		z1, _ := input.Int()
		x2, _ := input.Int()
		y2, _ := input.Int()
		z2, ok := input.Int()
		if !ok {
			break
		}
		bricks = append(bricks, [6]int{
			min(x1, x2),
			min(y1, y2),
			min(z1, z2),
			max(x1, x2),
			max(y1, y2),
			max(z1, z2),
		})
	}
	slices.SortFunc(bricks, func(a, b [6]int) int {
		return a[2] - b[2]
	})
	tower := make([][2]map[int]bool, len(bricks))
	tops := map[[2]int][2]int{}
	for i, brick := range bricks {
		tower[i] = [2]map[int]bool{map[int]bool{}, map[int]bool{}}
		bot := 0
		for x := brick[0]; x <= brick[3]; x++ {
			for y := brick[1]; y <= brick[4]; y++ {
				bot = max(bot, tops[[2]int{x, y}][1])
			}
		}
		topz := bot + brick[5] - brick[2] + 1
		for x := brick[0]; x <= brick[3]; x++ {
			for y := brick[1]; y <= brick[4]; y++ {
				xy := [2]int{x, y}
				top, ok := tops[xy]
				tops[xy] = [2]int{i, topz}
				if ok && top[1] == bot {
					tower[i][0][top[0]] = true
					tower[top[0]][1][i] = true
				}
			}
		}
	}
	return tower
}

func (aoc *aoc202322) Part1(input *Input) string {
	tower := aoc.parse(input)

	result := 0
	for _, brick := range tower {
		safe := true
		for above := range brick[1] {
			if len(tower[above][0]) == 1 {
				safe = false
				break
			}
		}
		if safe {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202322) falls(tower [][2]map[int]bool, i int) int {
	falls := map[int]bool{}
	falls[i] = true
	queue := NewQueue[int]()
	for above := range tower[i][1] {
		queue.Enqueue(above)
	}
	for !queue.Empty() {
		i := queue.Dequeue()
		if falls[i] {
			continue
		}
		if len(tower[i][0]) == 0 {
			continue
		}
		supported := false
		for below := range tower[i][0] {
			if !falls[below] {
				supported = true
				break
			}
		}
		if supported {
			continue
		}
		falls[i] = true
		for above := range tower[i][1] {
			if !falls[above] {
				queue.Enqueue(above)
			}
		}
	}
	return len(falls) - 1
}

func (aoc *aoc202322) Part2(input *Input) string {
	tower := aoc.parse(input)

	in := make(chan int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for i := range in {
				result += aoc.falls(tower, i)
			}
			out <- result
		}()
	}
	for i := range tower {
		in <- i
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
