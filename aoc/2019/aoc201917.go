package main

import (
	"os"
)

func makeMap(ic *intcode) (int64, [2]int, [2]int, map[[2]int]int64) {
	xy := [2]int{0, 0}

	robot := int64(0)
	robotxy := xy
	xymax := xy
	m := map[[2]int]int64{}
	ic.init()
	ic.interpIO(func() (bool, int64) {
		return false, 0
	}, func(out int64) bool {
		switch out {
		case '\n':
			xy = [2]int{0, xy[1] + 1}
			xymax[1] = xy[1]
			return false
		case 'X':
			robotxy = xy
		case '^', 'v', '<', '>':
			robot = out
			robotxy = xy
			m[xy] = '#'
		case '#':
			m[xy] = out
		}
		xy[0]++
		if xy[0] > xymax[0] {
			xymax[0] = xy[0]
		}
		return false
	})
	return robot, robotxy, xymax, m
}

func part1(robot int64, robotxy [2]int, xymax [2]int, m map[[2]int]int64) int {
	sum := 0
	for x := 1; x < xymax[0]; x++ {
		for y := 1; y < xymax[1]; y++ {
			if m[[2]int{x, y}] == '#' && m[[2]int{x + 1, y}] == '#' && m[[2]int{x - 1, y}] == '#' && m[[2]int{x, y + 1}] == '#' && m[[2]int{x, y - 1}] == '#' {
				sum += x * y
			}
		}
	}
	return sum
}

func part2prep(robot int64, robotxy [2]int, xymax [2]int, m map[[2]int]int64) {
	for y := 0; y <= xymax[1]; y++ {
		for x := 0; x <= xymax[0]; x++ {
			xy := [2]int{x, y}
			loc := "."
			if m[xy] == '#' {
				loc = "#"
			}
			if xy == robotxy {
				loc = string([]byte{byte(robot)})
			}
			print(loc)
		}
		println()
	}

	add := func(xy, dxy [2]int) [2]int {
		return [2]int{xy[0]+dxy[0],xy[1]+dxy[1]}
	}
	turnL := func(dxy [2]int) [2]int {
		return [2]int{dxy[1],-dxy[0]}
	}
	turnR := func(dxy [2]int) [2]int {
		return [2]int{-dxy[1],dxy[0]}
	}
	dxy := [2]int{1, 0}
	switch robot {
	case '^': dxy = [2]int{0, -1}
	case '<': dxy = [2]int{-1, 0}
	case '>': dxy = [2]int{1, 0}
	case 'v': dxy = [2]int{0, 1}
	}
	xy := robotxy
	for {
		if m[add(xy,turnL(dxy))] == '#' {
			dxy = turnL(dxy)
			print("L,")
		} else if m[add(xy,turnR(dxy))] == '#' {
			dxy = turnR(dxy)
			print("R,")
		} else {
			break
		}
		n := 0
		for m[add(xy,dxy)] == '#' {
			n++
			xy = add(xy,dxy)
		}
		print(n,",")
	}
	println()
}

func part2(ic *intcode) int64 {
	// Worked out by hand for my input data:
	// Main:A,B,A,C,B,A,C,B,A,C
	// A:L,12,L,12,L,6,L,6
	// B:R,8,R,4,L,12
	// C:L,12,L,6,R,12,R,8

	input := []byte("A,B,A,C,B,A,C,B,A,C\nL,12,L,12,L,6,L,6\nR,8,R,4,L,12\nL,12,L,6,R,12,R,8\nn\n")
	result := int64(0)

	ic.init()
	ic.mem[0] = 2
	ic.interpIO(func() (bool, int64) {
		in := int64(input[0])
		input = input[1:]
		return false, in
	}, func(out int64) bool {
		if out > 128 {
			result = out
		}
		return false
	})
	return result
}

func main() {
	ic := parseFile("input/17.txt")

	// Part 1:
	println(part1(makeMap(ic)))

	// Part 2:
	if len(os.Args) < 2 {
		println(part2(ic))
	} else {
		part2prep(makeMap(ic))
	}
}
