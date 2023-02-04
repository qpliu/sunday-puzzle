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
