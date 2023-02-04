package main

type Robot struct {
	x, y   int
	dx, dy int
	xmin, xmax, ymin, ymax int

	expectTurn      bool
	paint           map[[2]int]bool // black = false, white = true
	whitePaintCount int
	blackPaintCount int
}

func (robot *Robot) run(ic *intcode, start bool) {
	robot.x = 0
	robot.y = 0
	robot.dx = 0
	robot.dy = -1
	robot.xmin = 0
	robot.xmax = 0
	robot.ymin = 0
	robot.ymax = 0

	robot.paint = map[[2]int]bool{[2]int{0,0}:start}

	ic.init()
	ic.interpIO(func() (bool, int64) {
		if robot.paint[[2]int{robot.x, robot.y}] {
			return false, int64(1)
		} else {
			return false, int64(0)
		}
	}, func(out int64) bool {
		if robot.expectTurn {
			if out == 0 {
				robot.dx, robot.dy = -robot.dy, robot.dx
			} else {
				robot.dx, robot.dy = robot.dy, -robot.dx
			}
			robot.x += robot.dx
			robot.y += robot.dy
			if robot.x < robot.xmin {
				robot.xmin = robot.x
			}
			if robot.x > robot.xmax {
				robot.xmax = robot.x
			}
			if robot.y < robot.ymin {
				robot.ymin = robot.y
			}
			if robot.y > robot.ymax {
				robot.ymax = robot.y
			}
			robot.expectTurn = false
		} else {
			if out == 0 {
				robot.blackPaintCount++
				robot.paint[[2]int{robot.x, robot.y}] = false
			} else {
				robot.whitePaintCount++
				robot.paint[[2]int{robot.x, robot.y}] = true
			}
			robot.expectTurn = true
		}
		return false
	})
}

func main() {
	// Part 1
	ic := parseFile("input/11.txt")
	robot := &Robot{}
	robot.run(ic, false)
	println(len(robot.paint))

	// Part 2
	robot.run(ic, true)
	for y := robot.ymin; y <= robot.ymax; y++ {
		for x := robot.xmax; x >= robot.xmin; x-- {
			if robot.paint[[2]int{x,y}] {
				print("#")
			} else {
				print(".")
			}
		}
		println()
	}
}
