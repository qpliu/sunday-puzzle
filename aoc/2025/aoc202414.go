package main

func init() {
	Register(&aoc202414{
		AOC: AOC{
			Day:           14,
			InputFilename: "../2024/input/14.txt",
			Tests: []Test{
				Test{
					Input: `p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
`,
					Part1: "12",
					Part2: "",
				},
			},
		},
	})
}

type aoc202414 struct {
	AOC
}

func (aoc *aoc202414) parse(width, height int, input *Input) [][4]int {
	robots := [][4]int{}
	for {
		x, ok := input.Int()
		if !ok {
			return robots
		}
		y, _ := input.Int()
		vx, _ := input.Int()
		vy, _ := input.Int()
		for vx < 0 {
			vx += width
		}
		for vy < 0 {
			vy += height
		}
		robots = append(robots, [4]int{x, y, vx, vy})
	}
}

func (aoc *aoc202414) safetyFactor(width, height int, robots [][4]int, t int) int {
	nul := 0
	nur := 0
	nll := 0
	nlr := 0
	xc := width / 2
	yc := height / 2
	for _, robot := range robots {
		x := (robot[0] + t*robot[2]) % width
		y := (robot[1] + t*robot[3]) % height
		if x < xc {
			if y < yc {
				nul++
			} else if y > yc {
				nll++
			}
		} else if x > xc {
			if y < yc {
				nur++
			} else if y > yc {
				nlr++
			}
		}
	}
	return nul * nur * nll * nlr
}

func (aoc *aoc202414) draw(width, height int, robots [][4]int, t int) {
	grid := map[[2]int]bool{}
	for _, robot := range robots {
		grid[[2]int{
			(robot[0] + t*robot[2]) % width,
			(robot[1] + t*robot[3]) % height,
		}] = true
	}
	for y := range height {
		for x := range width {
			if grid[[2]int{x, y}] {
				print("#")
			} else {
				print(".")
			}
		}
		println()
	}
}

func (aoc *aoc202414) Test1(input *Input) string {
	const w = 11
	const h = 7
	const t = 100
	return IntResult(aoc.safetyFactor(w, h, aoc.parse(w, h, input), t))
}

func (aoc *aoc202414) Part1(input *Input) string {
	const w = 101
	const h = 103
	const t = 100
	return IntResult(aoc.safetyFactor(w, h, aoc.parse(w, h, input), t))
}

func (aoc *aoc202414) Part2(input *Input) string {
	const w = 101
	const h = 103
	robots := aoc.parse(w, h, input)

	// the horizontal positions recur every 101 seconds
	// the vertical positions recur every 103 seconds
	// the easter egg x positions are at nx + mx*101
	// the easter egg y positions are at ny + my*103
	// they converge when nx + mx*101 = ny + my*103
	t1 := [2]int{-1, -1}
	t2 := [2]int{-1, -1}
	for t := range max(w, h) {
		sf := aoc.safetyFactor(w, h, robots, t)

		if t1[0] < 0 || sf < t1[0] {
			t2 = t1
			t1 = [2]int{sf, t}
		} else if t2[0] < 0 || sf < t2[0] {
			t2 = [2]int{sf, t}
		}
	}
	t0 := t1 // in case nx = ny
	{
		t := Convergences([2]int{t1[1], w}, [2]int{t2[1], h})
		sf := aoc.safetyFactor(w, h, robots, t[0])
		if sf < t0[0] {
			t0 = [2]int{sf, t[0]}
		}
	}
	{
		t := Convergences([2]int{t1[1], h}, [2]int{t2[1], w})
		sf := aoc.safetyFactor(w, h, robots, t[0])
		if sf < t0[0] {
			t0 = [2]int{sf, t[0]}
		}
	}
	return IntResult(t0[1])
}
