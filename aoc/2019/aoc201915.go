/*
--- Day 15: Oxygen System ---

Out here in deep space, many things can go wrong. Fortunately, many of those
things have indicator lights. Unfortunately, one of those lights is lit: the
oxygen system for part of the ship has failed!

According to the readouts, the oxygen system must have failed days ago after a
rupture in oxygen tank two; that section of the ship was automatically sealed
once oxygen levels went dangerously low. A single remotely-operated repair
droid is your only option for fixing the oxygen system.

The Elves' care package included an Intcode program (your puzzle input) that
you can use to remotely control the repair droid. By running that program, you
can direct the repair droid to the oxygen system and fix the problem.

The remote control program executes the following steps in a loop forever:

 - Accept a movement command via an input instruction.
 - Send the movement command to the repair droid.
 - Wait for the repair droid to finish the movement operation.
 - Report on the status of the repair droid via an output instruction.

Only four movement commands are understood: north (1), south (2), west (3), and
east (4). Any other command is invalid. The movements differ in direction, but
not in distance: in a long enough east-west hallway, a series of commands like
4,4,4,4,3,3,3,3 would leave the repair droid back where it started.

The repair droid can reply with any of the following status codes:

 - 0: The repair droid hit a wall. Its position has not changed.
 - 1: The repair droid has moved one step in the requested direction.
 - 2: The repair droid has moved one step in the requested direction; its new
      position is the location of the oxygen system.

You don't know anything about the area around the repair droid, but you can
figure it out by watching the status codes.

For example, we can draw the area using D for the droid, # for walls, . for
locations the droid can traverse, and empty space for unexplored locations.
Then, the initial state looks like this:

|
|
|    D
|
|

To make the droid go north, send it 1. If it replies with 0, you know that
location is a wall and that the droid didn't move:

|
|    #
|    D
|
|

To move east, send 4; a reply of 1 means the movement was successful:

|
|    #
|    .D
|
|

Then, perhaps attempts to move north (1), south (2), and east (4) are all met
with replies of 0:

|
|    ##
|    .D#
|     #
|
Now, you know the repair droid is in a dead end. Backtrack with 3 (which you
already know will get a reply of 1 because you already know that location is
open):

|
|    ##
|    D.#
|     #
|

Then, perhaps west (3) gets a reply of 0, south (2) gets a reply of 1, south
again (2) gets a reply of 0, and then west (3) gets a reply of 2:

|
|    ##
|   #..#
|   D.#
|    #

Now, because of the reply of 2, you know you've found the oxygen system! In
this example, it was only 2 moves away from the repair droid's starting
position.

What is the fewest number of movement commands required to move the repair
droid from its starting position to the location of the oxygen system?
*/

package main

const (
	north = 1
	south = 2
	west  = 3
	east  = 4

	wall  = 0
	moved = 1
	goal  = 2

	nKnown   = 1
	sKnown   = 2
	wKnown   = 4
	eKnown   = 8

	nWall    = 16
	sWall    = 32
	wWall    = 64
	eWall    = 128
)

// depth-first
func makeMap(ic *intcode) ([2]int, map[[2]int]byte) {
	goalxy := [2]int{0, 0}
	xy := [2]int{0, 0}
	command := int64(1)
	backtrack := []int64{}
	m := map[[2]int]byte{}
	xymin := [2]int{0, 0}
	xymax := [2]int{0, 0}
	backtracking := false
	ic.init()
	ic.interpIO(func() (bool, int64) {
		return false, command
	}, func(out int64) bool {
		switch out {
		case wall:
			m[xy] |= (1 << byte(command-1)) | (8 << byte(command))
		case goal, moved:
			m[xy] |= (1 << byte(command-1))
			switch command {
			case north:
				xy[1]--
				if xy[1] < xymin[1] {
					xymin[1] = xy[1]
				}
			case south:
				xy[1]++
				if xy[1] > xymax[1] {
					xymax[1] = xy[1]
				}
			case west:
				xy[0]--
				if xy[0] < xymin[0] {
					xymin[0] = xy[0]
				}
			case east:
				xy[0]++
				if xy[0] > xymax[0] {
					xymax[0] = xy[0]
				}
			}
			m[xy] |= (1 << byte((command-1)^1))
			if !backtracking {
				backtrack = append(backtrack, (command-1)^1+1)
			}
			if out == goal {
				goalxy = xy
			}
		}
		mxy := m[xy]
		if mxy&nKnown == 0 {
			command = north
			backtracking = false
		} else if mxy&sKnown == 0 {
			command = south
			backtracking = false
		} else if mxy&wKnown == 0 {
			command = west
			backtracking = false
		} else if mxy&eKnown == 0 {
			command = east
			backtracking = false
		} else if len(backtrack) > 0 {
			command = backtrack[len(backtrack)-1]
			backtrack = backtrack[:len(backtrack)-1]
			backtracking = true
		} else {
			return true
		}
		return false
	})
	println("goal:",goalxy[0]," ", goalxy[1]," size:", len(m)," xmin:", xymin[0]," ymin:", xymin[1])
	for y := xymin[1]; y <= xymax[1]; y++ {
		for x := xymin[0]; x <= xymax[0]; x++ {
			if x == 0 && y == 0 {
				print("0")
				continue
			} else if x == goalxy[0] && y == goalxy[1] {
				print("*")
				continue
			}
			switch m[[2]int{x,y}] {
			case 0x0f:
				print("┼")
			case 0x1f:
				print("┬")
			case 0x2f:
				print("┴")
			case 0x4f:
				print("├")
			case 0x8f:
				print("┤")
			case 0x3f:
				print("─")
			case 0x5f:
				print("┌")
			case 0x9f:
				print("┐")
			case 0x6f:
				print("└")
			case 0xaf:
				print("┘")
			case 0xcf:
				print("│")
			case 0xef, 0xdf, 0xbf, 0x7f:
				print(".")
			case 0xff:
				print("X")
			default:
				print("?")
			}
		}
		println()
	}
	return goalxy, m
}

// breadth-first
func search(goalxy [2]int, m map[[2]int]byte) int {
	moves := 0
	current := [][2]int{[2]int{0, 0}}
	next := [][2]int{}
	for {
		for _, xy := range current {
			if xy == goalxy {
				return moves
			}
			mxy := m[xy]
			if mxy&1 == 0 {
				continue
			}
			m[xy] &= 254
			if mxy&nWall == 0 {
				next = append(next, [2]int{xy[0], xy[1] - 1})
			}
			if mxy&sWall == 0 {
				next = append(next, [2]int{xy[0], xy[1] + 1})
			}
			if mxy&wWall == 0 {
				next = append(next, [2]int{xy[0] - 1, xy[1]})
			}
			if mxy&eWall == 0 {
				next = append(next, [2]int{xy[0] + 1, xy[1]})
			}
		}
		moves++
		current, next = next, current[:0]
	}
}

func fill(goalxy [2]int, m map[[2]int]byte) int {
	moves := 0
	current := [][2]int{goalxy}
	next := [][2]int{}
	for {
		for _, xy := range current {
			mxy := m[xy]
			if mxy&2 == 0 {
				continue
			}
			m[xy] &= 253
			if mxy&nWall == 0 && m[[2]int{xy[0], xy[1] - 1}]&2 != 0{
				next = append(next, [2]int{xy[0], xy[1] - 1})
			}
			if mxy&sWall == 0 && m[[2]int{xy[0], xy[1] + 1}]&2 != 0 {
				next = append(next, [2]int{xy[0], xy[1] + 1})
			}
			if mxy&wWall == 0 && m[[2]int{xy[0] - 1, xy[1]}]&2 != 0 {
				next = append(next, [2]int{xy[0] - 1, xy[1]})
			}
			if mxy&eWall == 0 && m[[2]int{xy[0] + 1, xy[1]}]&2 != 0 {
				next = append(next, [2]int{xy[0] + 1, xy[1]})
			}
		}
		if len(next) == 0 {
			return moves
		}
		moves++
		current, next = next, current[:0]
	}
}

func main() {
	goalxy, m := makeMap(parseFile("input/15.txt"))

	// Part 1
	println(search(goalxy, m))

	// Part 2
	println(fill(goalxy, m))
}
