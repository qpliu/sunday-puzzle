/*
--- Day 11: Space Police ---

On the way to Jupiter, you're pulled over by the Space Police.

"Attention, unmarked spacecraft! You are in violation of Space Law! All
spacecraft must have a clearly visible registration identifier! You have 24
hours to comply or be sent to Space Jail!"

Not wanting to be sent to Space Jail, you radio back to the Elves on Earth for
help. Although it takes almost three hours for their reply signal to reach you,
they send instructions for how to power up the emergency hull painting robot
and even provide a small Intcode program (your puzzle input) that will cause it
to paint your ship appropriately.

There's just one problem: you don't have an emergency hull painting robot.

You'll need to build a new emergency hull painting robot. The robot needs to be
able to move around on the grid of square panels on the side of your ship,
detect the color of its current panel, and paint its current panel black or
white. (All of the panels are currently black.)

The Intcode program will serve as the brain of the robot. The program uses
input instructions to access the robot's camera: provide 0 if the robot is over
a black panel or 1 if the robot is over a white panel. Then, the program will
output two values:

 - First, it will output a value indicating the color to paint the panel the
   robot is over: 0 means to paint the panel black, and 1 means to paint the
   panel white.
 - Second, it will output a value indicating the direction the robot should
   turn: 0 means it should turn left 90 degrees, and 1 means it should turn
   right 90 degrees.

After the robot turns, it should always move forward exactly one panel. The
robot starts facing up.

The robot will continue running for a while like this and halt when it is
finished drawing. Do not restart the Intcode computer inside the robot during
this process.

For example, suppose the robot is about to start running. Drawing black panels
as ., white panels as #, and the robot pointing the direction it is facing (< ^
> v), the initial state and region near the robot looks like this:

| .....
| .....
| ..^..
| .....
| .....

The panel under the robot (not visible here because a ^ is shown instead) is
also black, and so any input instructions at this point should be provided 0.
Suppose the robot eventually outputs 1 (paint white) and then 0 (turn left).
After taking these actions and moving forward one panel, the region now looks
like this:

| .....
| .....
| .<#..
| .....
| .....

Input instructions should still be provided 0. Next, the robot might output 0
(paint black) and then 0 (turn left):

| .....
| .....
| ..#..
| .v...
| .....

After more outputs (1,0, 1,0):

| .....
| .....
| ..^..
| .##..
| .....

The robot is now back where it started, but because it is now on a white panel,
input instructions should be provided 1. After several more outputs (0,1, 1,0,
1,0), the area looks like this:

| .....
| ..<#.
| ...#.
| .##..
| .....

Before you deploy the robot, you should probably have an estimate of the area
it will cover: specifically, you need to know the number of panels it paints at
least once, regardless of color. In the example above, the robot painted 6
panels at least once. (It painted its starting panel twice, but that panel is
still only counted once; it also never painted the panel it ended on.)

Build a new emergency hull painting robot and run the Intcode program on it.
How many panels does it paint at least once?
*/

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
