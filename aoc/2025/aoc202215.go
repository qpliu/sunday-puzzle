package main

import (
	"math"
	"runtime"
	"slices"
)

func init() {
	Register(&aoc202215{
		AOC: AOC{
			Day:           15,
			InputFilename: "../2022/input/15.txt",
			Tests: []Test{
				Test{
					Input: `Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
`,
					Part1: "26",
					Part2: "56000011",
				},
			},
		},
	})
}

type aoc202215 struct {
	AOC
}

func (aoc *aoc202215) sensors(input *Input) Seq[[4]int] {
	return func(yield func([4]int) bool) {
		for {
			xs, _ := input.Int()
			ys, _ := input.Int()
			xb, _ := input.Int()
			yb, ok := input.Int()
			if !ok {
				return
			}
			if !yield([4]int{xs, ys, xb, yb}) {
				return
			}
		}
	}
}

func (aoc *aoc202215) part1(y int, input *Input) string {
	impossible := [][2]int{}
	for sensor := range aoc.sensors(input) {
		xs, ys, xb, yb := sensor[0], sensor[1], sensor[2], sensor[3]
		r := max(xs-xb, xb-xs) + max(ys-yb, yb-ys) - max(y-ys, ys-y)
		if r < 0 {
		} else if yb != y {
			impossible = append(impossible, [2]int{xs - r, xs + r})
		} else if xs < xb {
			impossible = append(impossible, [2]int{xs - r, xb - 1})
		} else if xs > xb {
			impossible = append(impossible, [2]int{xb + 1, xs + r})
		}
	}

	slices.SortFunc(impossible, func(a, b [2]int) int {
		return a[0] - b[0]
	})

	result := 0
	x := math.MinInt
	for _, r := range impossible {
		x = max(x, r[0])
		if r[1] >= x {
			result += r[1] - x + 1
			x = r[1] + 1
		}
	}
	return IntResult(result)
}

func (aoc *aoc202215) Test1(input *Input) string {
	return aoc.part1(10, input)
}

func (aoc *aoc202215) Part1(input *Input) string {
	return aoc.part1(2000000, input)
}

func (aoc *aoc202215) parse2(xymax int, input *Input) ([]int, []int, [][3]int) {
	xset, yset := map[int]bool{}, map[int]bool{}
	sensors := [][3]int{}
	for sensor := range aoc.sensors(input) {
		xs, ys, xb, yb := sensor[0], sensor[1], sensor[2], sensor[3]
		xset[xs] = true
		yset[ys] = true
		sensors = append(sensors, [3]int{xs, ys, max(xs-xb, xb-xs) + max(ys-yb, yb-ys)})
	}

	xs := []int{}
	xset[0] = true
	xset[xymax] = true
	for x := range xset {
		xs = append(xs, x)
	}
	slices.Sort(xs)

	ys := []int{}
	yset[0] = true
	yset[xymax] = true
	for y := range yset {
		ys = append(ys, y)
	}
	slices.Sort(ys)

	return xs, ys, sensors
}

func (aoc *aoc202215) scanRegion(sensors [][3]int, in chan [4]int, out chan [][2]int) {
	result := [][2]int{}
regionLoop:
	for region := range in {
		x0, y0, x1, y1 := region[0], region[1], region[2], region[3]
		width := x1 - x0 + y1 - y0
		maxDR, maxDL, maxUL, maxUR := -1, -1, -1, -1
		for _, sensor := range sensors {
			x, y, dist := sensor[0], sensor[1], sensor[2]
			switch [4]bool{x <= x0, y <= y0, x >= x1, y >= y1} {
			case [4]bool{true, true, false, false}:
				// upper-left
				maxUL = max(maxUL, dist-(x0-x)-(y0-y))
			case [4]bool{true, false, false, true}:
				// lower-left
				maxDL = max(maxDL, dist-(x0-x)-(y-y1))
				if maxDL+maxUR >= width-1 {
					continue regionLoop
				}
			case [4]bool{false, false, true, true}:
				// lower-right
				maxDR = max(maxDR, dist-(x-x1)-(y-y1))
				if maxDR+maxUL >= width-1 {
					continue regionLoop
				}
			case [4]bool{false, true, true, false}:
				// upper-right
				maxUR = max(maxUR, dist-(x-x1)-(y0-y))
				if maxUR+maxDL >= width-1 {
					continue regionLoop
				}
			default:
				panic("?")
			}
		}
		// ux - x0 + uy - y0 = maxUL
		// x1 - ux + uy - y0 = maxUR
		ux := (maxUL - maxUR + x0 + x1) / 2
		uy := (maxUL + maxUR + 2*y0 + x0 - x1) / 2
		// lx - x0 + ly - y0 = maxUL
		// lx - x0 + y1 - ly = maxDL
		lx := (maxUL + maxDL + 2*x0 + y0 - y1) / 2
		ly := (maxUL - maxDL + y0 + y1) / 2
		// x1 - rx + ry - y0 = maxUR
		// x1 - rx + y1 - ry = maxDR
		rx := (-maxUR - maxDR + 2*x1 - y0 + y1) / 2
		ry := (maxUR - maxDR + y0 + y1) / 2
		// dx - x0 + y1 - dy = maxDL
		// x1 - dx + y1 - dy = maxDR
		dx := (maxDL - maxDR + x0 + x1) / 2
		dy := (-maxDL - maxDR + 2*y1 - x0 + x1) / 2

		if dx == ux && ly == ry && uy+1 == ly && dy-1 == ly && lx+1 == ux && rx-1 == ux {
			result = append(result, [2]int{ux, ly})
		}
	}
	out <- result
}

func (aoc *aoc202215) part2(xymax int, input *Input) string {
	xs, ys, sensors := aoc.parse2(xymax, input)
	in := make(chan [4]int)
	out := make(chan [][2]int)
	for range runtime.NumCPU() {
		go aoc.scanRegion(sensors, in, out)
	}

	lasty := ys[0]
	for _, y := range ys[1:] {
		lastx := xs[0]
		for _, x := range xs[1:] {
			in <- [4]int{lastx, lasty, x, y}
			lastx = x
		}
		lasty = y
	}
	close(in)

	beacons := map[[2]int]bool{}
	for range runtime.NumCPU() {
		for _, beacon := range <-out {
			beacons[beacon] = true
		}
	}

	if len(beacons) != 1 {
		return "bad input"
	}

	result := 0
	for beacon := range beacons {
		result += beacon[0]*4000000 + beacon[1]
	}
	return IntResult(result)
}

func (aoc *aoc202215) Test2(input *Input) string {
	return aoc.part2(20, input)
}

func (aoc *aoc202215) Part2(input *Input) string {
	return aoc.part2(400000, input)
}
