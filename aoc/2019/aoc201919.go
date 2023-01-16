/*
--- Day 19: Tractor Beam ---

Unsure of the state of Santa's ship, you borrowed the tractor beam technology
from Triton. Time to test it out.

When you're safely away from anything else, you activate the tractor beam, but
nothing happens. It's hard to tell whether it's working if there's nothing to
use it on. Fortunately, your ship's drone system can be configured to deploy a
drone to specific coordinates and then check whether it's being pulled. There's
even an Intcode program (your puzzle input) that gives you access to the drone
system.

The program uses two input instructions to request the X and Y position to
which the drone should be deployed. Negative numbers are invalid and will
confuse the drone; all numbers should be zero or positive.

Then, the program will output whether the drone is stationary (0) or being
pulled by something (1). For example, the coordinate X=0, Y=0 is directly in
front of the tractor beam emitter, so the drone control program will always
report 1 at that location.

To better understand the tractor beam, it is important to get a good picture of
the beam itself. For example, suppose you scan the 10x10 grid of points closest
to the emitter:

|        X
|   0->      9
|  0#.........
|  |.#........
|  v..##......
|   ...###....
|   ....###...
| Y .....####.
|   ......####
|   ......####
|   .......###
|  9........##

In this example, the number of points affected by the tractor beam in the 10x10
area closest to the emitter is 27.

However, you'll need to scan a larger area to understand the shape of the beam.
How many points are affected by the tractor beam in the 50x50 area closest to
the emitter? (For each of X and Y, this will be 0 through 49.)
*/

package main

func part1(ic *intcode) int64 {
	count := int64(0)
	for x := int64(0); x < 50; x++ {
		for y := int64(0); y < 50; y++ {
			ic.init()
			out := ic.interp([]int64{x, y})
			count += out[0]
		}
	}
	return count
}

func part2(ic *intcode) int {
	spans := [][3]int{} // y, x0, x1
	spanMaxX0 := 0
	spanMinX1 := 0
	lastX0 := 2
	for y := 2; y < 10000; y++ {
		hasX0 := false
		x0 := 0
		x1 := 0
		for x := lastX0; x < 10000; x++ {
			ic.init()
			out := ic.interp([]int64{int64(x), int64(y)})
			if out[0] == 0 {
				if hasX0 {
					break
				}
			} else if !hasX0 {
				hasX0 = true
				x0 = x
				x1 = x + 1
			} else {
				x1 = x + 1
			}
		}
		if !hasX0 {
			spans = spans[:0]
			continue
		}
		lastX0 = x0
		if x1 - x0 < 100 {
			spans = spans[:0]
			continue
		}
		if len(spans) == 0 {
			spanMaxX0 = x0
			spanMinX1 = x1
		} else {
			if spanMaxX0 < x0 {
				spanMaxX0 = x0
			}
			if spanMinX1 > x1 {
				spanMinX1 = x1
			}
		}
		spans = append(spans, [3]int{y, x0, x1})
		for spanMinX1 - spanMaxX0 < 100 {
			spans = spans[1:]
			spanMaxX0 = spans[0][1]
			spanMinX1 = spans[0][2]
			for _, yx01 := range spans {
				if spanMaxX0 < yx01[1] {
					spanMaxX0 = yx01[1]
				} else if spanMinX1 > yx01[2] {
					spanMinX1 = yx01[2]
				}
			}
		}
		if len(spans) < 100 {
			continue
		}
		return spanMaxX0*10000+spans[0][0]
	}
	panic("No result")
}

func main() {
	ic := parseFile("input/19.txt")
	println(part1(ic))
	println(part2(ic))
}
