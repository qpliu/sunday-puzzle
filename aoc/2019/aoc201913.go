/*
-- Day 13: Care Package ---

As you ponder the solitude of space and the ever-increasing three-hour
roundtrip for messages between you and Earth, you notice that the Space Mail
Indicator Light is blinking. To help keep you sane, the Elves have sent you a
care package.

It's a new game for the ship's arcade cabinet! Unfortunately, the arcade is all
the way on the other end of the ship. Surely, it won't be hard to build your
own - the care package even comes with schematics.

The arcade cabinet runs Intcode software like the game the Elves sent (your
puzzle input). It has a primitive screen capable of drawing square tiles on a
grid. The software draws tiles to the screen with output instructions: every
three output instructions specify the x position (distance from the left), y
position (distance from the top), and tile id. The tile id is interpreted as
follows:

 - 0 is an empty tile. No game object appears in this tile.
 - 1 is a wall tile. Walls are indestructible barriers.
 - 2 is a block tile. Blocks can be broken by the ball.
 - 3 is a horizontal paddle tile. The paddle is indestructible.
 - 4 is a ball tile. The ball moves diagonally and bounces off objects.

For example, a sequence of output values like 1,2,3,6,5,4 would draw a
horizontal paddle tile (1 tile from the left and 2 tiles from the top) and a
ball tile (6 tiles from the left and 5 tiles from the top).

Start the game. How many block tiles are on the screen when the game exits?
*/

package main

func part1() {
	screen := map[[2]int64]int64{}
	ic := parseFile("input/13.txt")
	x, y, state := int64(0), int64(0), 0
	ic.init()
	ic.interpIO(func() (bool, int64) {
		return false, 0
	}, func(out int64) bool {
		switch state {
		case 0:
			x, state = out, 1
		case 1:
			y, state = out, 2
		case 2:
			screen[[2]int64{x, y}], state = out, 0
		}
		return false
	})
	count := 0
	for _, tile := range screen {
		if tile == 2 {
			count++
		}
	}
	println(count)
}

func part2() {
	screen := map[[2]int64]int64{}
	ic := parseFile("input/13.txt")
	x, y, score, state := int64(0), int64(0), int64(0), 0
	ballx, paddlex := int64(0), int64(0)

	ic.code[0] = 2
	ic.init()
	ic.interpIO(func() (bool, int64) {
		for {
			if paddlex > ballx {
				return false, -1
			} else if paddlex == ballx {
				return false, 0
			} else {
				return false, 1
			}
		}
	}, func(out int64) bool {
		switch state {
		case 0:
			x, state = out, 1
		case 1:
			y, state = out, 2
		case 2:
			if x == -1 && y == 0 {
				score = out
			} else {
				screen[[2]int64{x, y}] = out
				switch out {
				case 3:
					paddlex = x
				case 4:
					ballx = x
				}
			}
			state = 0
		}
		return false
	})
	println(score)
}

func main() {
	part1()
	part2()
}
