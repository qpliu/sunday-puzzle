/*
--- Day 17: Reservoir Research ---

You arrive in the year 18. If it weren't for the coat you got in 1018, you
would be very cold: the North Pole base hasn't even been constructed.

Rather, it hasn't been constructed yet. The Elves are making a little progress,
but there's not a lot of liquid water in this climate, so they're getting very
dehydrated. Maybe there's more underground?

You scan a two-dimensional vertical slice of the ground nearby and discover
that it is mostly sand with veins of clay. The scan only provides data with a
granularity of square meters, but it should be good enough to determine how
much water is trapped there. In the scan, x represents the distance to the
right, and y represents the distance down. There is also a spring of water near
the surface at x=500, y=0. The scan identifies which square meters are clay
(your puzzle input).

For example, suppose your scan shows the following veins of clay:

| x=495, y=2..7
| y=7, x=495..501
| x=501, y=3..7
| x=498, y=2..4
| x=506, y=1..2
| x=498, y=10..13
| x=504, y=10..13
| y=13, x=498..504

Rendering clay as #, sand as ., and the water spring as +, and with x
increasing to the right and y increasing downward, this becomes:

|    44444455555555
|    99999900000000
|    45678901234567
|  0 ......+.......
|  1 ............#.
|  2 .#..#.......#.
|  3 .#..#..#......
|  4 .#..#..#......
|  5 .#.....#......
|  6 .#.....#......
|  7 .#######......
|  8 ..............
|  9 ..............
| 10 ....#.....#...
| 11 ....#.....#...
| 12 ....#.....#...
| 13 ....#######...

The spring of water will produce water forever. Water can move through sand,
but is blocked by clay. Water always moves down when possible, and spreads to
the left and right otherwise, filling space that has clay on both sides and
falling out otherwise.

For example, if five squares of water are created, they will flow downward
until they reach the clay and settle there. Water that has come to rest is
shown here as ~, while sand through which water has passed (but which is now
dry again) is shown as |:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#.|#......
| .#..#.|#......
| .#....|#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

Two squares of water can't occupy the same location. If another five squares of
water are created, they will settle on the first five, filling the clay
reservoir a little more:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#.|#......
| .#..#.|#......
| .#~~~~~#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

Water pressure does not apply in this scenario. If another four squares of
water are created, they will stay on the right side of the barrier, and no
water will reach the left side:

| ......+.......
| ......|.....#.
| .#..#.|.....#.
| .#..#~~#......
| .#..#~~#......
| .#~~~~~#......
| .#~~~~~#......
| .#######......
| ..............
| ..............
| ....#.....#...
| ....#.....#...
| ....#.....#...
| ....#######...

At this point, the top reservoir overflows. While water can reach the tiles
above the surface of the water, it cannot settle there, and so the next five
squares of water settle like this:

| ......+.......
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ........|.....
| ....#...|.#...
| ....#...|.#...
| ....#~~~~~#...
| ....#######...

Note especially the leftmost |: the new squares of water can reach this tile,
but cannot stop there. Instead, eventually, they all fall to the right and
settle in the reservoir below.

After 10 more squares of water, the bottom reservoir is also full:

| ......+.......
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ........|.....
| ....#~~~~~#...
| ....#~~~~~#...
| ....#~~~~~#...
| ....#######...

Finally, while there is nowhere left for the water to settle, it can reach a
few more tiles before overflowing beyond the bottom of the scanned data:

| ......+.......    (line not counted: above minimum y value)
| ......|.....#.
| .#..#||||...#.
| .#..#~~#|.....
| .#..#~~#|.....
| .#~~~~~#|.....
| .#~~~~~#|.....
| .#######|.....
| ........|.....
| ...|||||||||..
| ...|#~~~~~#|..
| ...|#~~~~~#|..
| ...|#~~~~~#|..
| ...|#######|..
| ...|.......|..    (line not counted: below maximum y value)
| ...|.......|..    (line not counted: below maximum y value)
| ...|.......|..    (line not counted: below maximum y value)

How many tiles can be reached by the water? To prevent counting forever, ignore
tiles with a y coordinate smaller than the smallest y coordinate in your scan
data or larger than the largest one. Any x coordinate is valid. In this
example, the lowest y coordinate given is 1, and the highest is 13, causing the
water spring (in row 0) and the water falling off the bottom of the render (in
rows 14 through infinity) to be ignored.

So, in the example above, counting both water at rest (~) and other sand tiles
the water can hypothetically reach (|), the total number of tiles the water can
reach is 57.

How many tiles can the water reach within the range of y values in your scan?
*/

// This is still super slow, but it eventually gets the results.

package main

import (
	"fmt"
	"io"
	"os"
	"strings"
)

func parse(r io.Reader) [][4]int {
	veins := [][4]int{}
	for {
		var xy, yx rune
		var a, b, c int
		if n, _ := fmt.Fscanf(r, "%c=%d, %c=%d..%d\n", &xy, &a, &yx, &b, &c); n != 5 {
			return veins
		}
		if xy == 'x' {
			veins = append(veins, [4]int{a, a, b, c})
		} else {
			veins = append(veins, [4]int{b, c, a, a})
		}
	}
}

type Ground struct {
	xmin, xmax, ymin, ymax int
	ground                 []byte
	stagnant, flowing      int
}

func (g *Ground) get(x, y int) byte {
	if x < g.xmin || x > g.xmax || y < g.ymin || y > g.ymax {
		return 0
	}
	i := x - g.xmin + (y-g.ymin)*(g.xmax-g.xmin+1)
	return g.ground[i]
}

func (g *Ground) set(x, y int, b byte) {
	if x < g.xmin || x > g.xmax || y < g.ymin || y > g.ymax {
		panic(fmt.Sprintf("xy=%d,%d bounds=(%d,%d),(%d,%d)", x, y, g.xmin, g.ymin, g.xmax, g.ymax))
	}
	i := x - g.xmin + (y-g.ymin)*(g.xmax-g.xmin+1)
	if g.ground[i] == '|' {
		g.flowing--
	} else if g.ground[i] == '~' {
		g.stagnant--
	}
	if b == '|' {
		g.flowing++
	} else if b == '~' {
		g.stagnant++
	}
	g.ground[i] = b
}

func makeGround(veins [][4]int) *Ground {
	xmin := 500
	xmax := 500
	ymin := 4294967295
	ymax := 0
	for _, vein := range veins {
		if vein[0]-1 < xmin {
			xmin = vein[0] - 1
		}
		if vein[1]+1 > xmax {
			xmax = vein[1] + 1
		}
		if vein[2] < ymin {
			ymin = vein[2] - 1
		}
		if vein[3] > ymax {
			ymax = vein[3]
		}
	}
	g := &Ground{xmin, xmax, ymin, ymax, make([]byte, (xmax-xmin+1)*(ymax-ymin+1)), 0, 0}
	for _, vein := range veins {
		for y := vein[2]; y <= vein[3]; y++ {
			for x := vein[0]; x <= vein[1]; x++ {
				g.set(x, y, '#')
			}
		}
	}
	return g
}

func (g *Ground) flow(x0, y0 int) bool {
	x := x0
	y := y0
	if y < g.ymin {
		y = g.ymin
	}
	updated := false
	for {
		if y > g.ymax {
			return updated
		}
		here := g.get(x, y)
		if here == 0 {
			updated = true
			g.set(x, y, '|')
		} else if here == '~' || here == '#' {
			// why would it get here?
			return updated
		}
		below := g.get(x, y+1)
		if below == '#' || below == '~' {
			break
		}
		y++
	}
	if updated {
		// trying to debug getting stuck in a loop
		return true
	}
	lbounded := g.bounded(x, y, -1)
	rbounded := g.bounded(x, y, 1)
	if lbounded && rbounded {
		for lbounded && rbounded {
			for i := 0; g.get(x+i, y) != '#'; i++ {
				g.set(x+i, y, '~')
			}
			for i := 1; g.get(x-i, y) != '#'; i++ {
				g.set(x-i, y, '~')
			}
			y--
			lbounded = g.bounded(x, y, -1)
			rbounded = g.bounded(x, y, 1)
		}
		return true
	}
	if g.flowx(x, y, -1) {
		return true
	}
	if g.flowx(x, y, 1) {
		return true
	}
	return false
}

func (g *Ground) bounded(x0, y0, dx int) bool {
	x := x0
	y := y0
	for {
		if g.get(x, y) == '#' {
			return true
		}
		below := g.get(x, y+1)
		if below == '#' || below == '~' {
			x += dx
		} else {
			return false
		}
	}
}

func (g *Ground) flowx(x0, y0, dx int) bool {
	x := x0
	y := y0
	updated := false
	for {
		here := g.get(x, y)
		if here == 0 {
			g.set(x, y, '|')
			updated = true
		} else if here == '#' {
			return updated
		}
		below := g.get(x, y+1)
		if below == '#' || below == '~' {
			x += dx
		} else if updated {
			return true
		} else {
			return g.flow(x, y)
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		const testData = "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"
		g := makeGround(parse(strings.NewReader(testData)))
		for g.flow(500, 0) {
		}
		println(g.stagnant, g.flowing)
		if g.stagnant+g.flowing != 57 {
			panic(fmt.Sprintf("%d,%d", g.stagnant, g.flowing))
		}
	} else {
		veins := func() [][4]int {
			f, err := os.Open("input/17.txt")
			if err != nil {
				panic(err.Error())
			}
			defer f.Close()
			return parse(f)
		}()
		g := makeGround(veins)
		for g.flow(500, 0) {
		}
		println(g.stagnant, g.flowing, g.stagnant+g.flowing)
	}
}
