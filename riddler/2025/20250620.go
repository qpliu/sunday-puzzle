package main

import (
	"bytes"
	"fmt"
	"math"
	"runtime"
)

func xyz(i, n int) (float64, float64, float64) {
	ix := i % (2*n + 1)
	iy := i / (2*n + 1) % (2*n + 1)
	iz := i / (2*n + 1) / (2*n + 1)
	return float64(ix-n) / float64(n), float64(iy-n) / float64(n), float64(iz-n) / float64(n)
}

type sphere struct {
	blocks []bool
	n      int
	total  int
}

func makeSphere(n int) sphere {
	size := (2*n + 1) * (2*n + 1) * (2*n + 1)
	blocks := make([]bool, size)
	total := 0
	for i := range blocks {
		x, y, z := xyz(i, n)
		blocks[i] = x*x+y*y+z*z <= 1.0
		if blocks[i] {
			total++
		}
	}
	return sphere{blocks, n, total}
}

func (s sphere) count() int {
	ncpu := runtime.NumCPU()
	size := len(s.blocks) / ncpu
	ch := make(chan int)
	for j := range ncpu {
		go func() {
			limit := min(len(s.blocks), (j+1)*size) - j*size
			c := 0
			for i := range limit {
				if s.blocks[i+j*size] {
					c++
				}
			}
			ch <- c
		}()
	}
	c := 0
	for _ = range ncpu {
		c += <-ch
	}
	return c
}

func inCylinder(x1, y1, z1, x2, y2, z2, r, x, y, z float64) bool {
	dx := x1 - x
	dy := y1 - y
	dz := z1 - z
	m := math.Sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1) + (z2-z1)*(z2-z1))
	nx := (x2 - x1) / m
	ny := (y2 - y1) / m
	nz := (z2 - z1) / m
	parm := dx*nx + dy*ny + dz*nz
	parx := parm * nx
	pary := parm * ny
	parz := parm * nz
	perpx := dx - parx
	perpy := dy - pary
	perpz := dz - parz
	return perpx*perpx+perpy*perpy+perpz*perpz <= r*r
}

func (s sphere) bore(x1, y1, z1, x2, y2, z2, r float64) {
	ncpu := runtime.NumCPU()
	size := len(s.blocks) / ncpu
	ch := make(chan bool)
	for j := range ncpu {
		go func() {
			limit := min(len(s.blocks), (j+1)*size) - j*size
			for i := range limit {
				x, y, z := xyz(i+j*size, s.n)
				if inCylinder(x1, y1, z1, x2, y2, z2, r, x, y, z) {
					s.blocks[i+j*size] = false
				}
			}
			ch <- false
		}()
	}
	for _ = range ncpu {
		<-ch
	}
}

func (s sphere) pass(x1, y1, z1, x2, y2, z2 float64, label string) {
	const r = 0.5
	s.bore(x1, y1, z1, x2, y2, z2, r)
	c := s.count()
	fmt.Printf("%s: %d/%d %f\n", label, c, s.total, float64(c)/float64(s.total))
}

func (s sphere) printLayer(z float64) {
	iz := int(z*float64(s.n)) + s.n
	b := bytes.Buffer{}
	fmt.Printf("Layer z=%f\n", z)
	for iy := range 2*s.n + 1 {
		b.Reset()
		for ix := range 2*s.n + 1 {
			if s.blocks[ix+(2*s.n+1)*iy+(2*s.n+1)*(2*s.n+1)*iz] {
				b.WriteString("#")
			} else {
				b.WriteString(".")
			}
		}
		println(b.String())
	}
}

func main() {
	if true {
		const n = 200
		s := makeSphere(n)
		s.pass(0, 0, 0, 0, 0, 1, "pass 1")
		s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
		s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")

		const delta4 = -0.0135
		s.pass(-1, 0.5+delta4, 1, 1, 0.5+delta4, -1, "diagonal face pass 4")
		s.pass(0.5+delta4, 1, 1, 0.5+delta4, -1, -1, "diagonal face pass 5")
		s.pass(1, -0.5-delta4, 1, -1, -0.5-delta4, -1, "diagonal face pass 6")
		s.pass(-0.5-delta4, -1, 1, -0.5-delta4, 1, -1, "diagonal face pass 7")
		s.pass(1, 1, 0, -1, -1, 0, "edge center diagonal pass 8")
		s.pass(1, -1, 0, -1, 1, 0, "edge center diagonal pass 9")
		s.pass(1, 0, 1, -1, 0, -1, "edge center diagonal pass 10")
		s.pass(1, 0, -1, -1, 0, 1, "edge center diagonal pass 11")
		s.pass(0, 1, 1, 0, -1, -1, "edge center diagonal pass 12")
		s.pass(0, 1, -1, 0, -1, 1, "edge center diagonal pass 13")
	}
	if false {
		// This convinces me that the second pass is centered
		{
			const n = 100
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0.01, 0, 0, 0.01, 1, 0, "offset perpendicular pass 2")
		}
		{
			const n = 100
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0.02, 0, 0, 0.02, 1, 0, "offset perpendicular pass 2")
		}
	}

	if false {
		// This convinces me that the second pass is perpendicular
		{
			const n = 100
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0.54, 0, 0, 0.54, 0, 1, "0.54 offset parallel pass 2")
		}

		{
			const n = 100
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0.545, 0, 0, 0.545, 0, 1, "0.545 offset parallel pass 2")
		}

		{
			const n = 100
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0.55, 0, 0, 0.55, 0, 1, "0.55 offset parallel pass 2")
		}
	}
	if false {
		// This convinces me that the fourth pass goes diagonally across a face
		{
			const n = 100
			const dx = 0.0
			const dy = 0.0
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
			s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
			s.pass(1+dx, 1+dy, 1, -1+dx, -1+dy, -1, fmt.Sprintf("diagonal pass 4: dx=%f dy=%f", dx, dy))
		}
		if false {
			{
				const n = 200
				const dx = 0.0
				const dy = 0.0
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(1+dx, 1+dy, 1, -1+dx, -1+dy, -1, fmt.Sprintf("diagonal pass 4: dx=%f dy=%f", dx, dy))
			}
			{
				const n = 200
				const dx = 0.1
				const dy = 0.0
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(1+dx, 1+dy, 1, -1+dx, -1+dy, -1, fmt.Sprintf("diagonal pass 4: dx=%f dy=%f", dx, dy))
			}
			{
				const n = 200
				const dx = 0.1
				const dy = 0.1
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(1+dx, 1+dy, 1, -1+dx, -1+dy, -1, fmt.Sprintf("diagonal pass 4: dx=%f dy=%f", dx, dy))
			}
		}

		{
			const n = 200
			const delta = 0.1400
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
			s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
			s.pass(0.5-delta, 0.5-delta, 0, 0.5-delta, 0.5-delta, 1, fmt.Sprintf("edge pass 4 delta=%f", delta))
		}

		if false {
			{
				const n = 200
				const delta = 0.1399
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5-delta, 0.5-delta, 0, 0.5-delta, 0.5-delta, 1, fmt.Sprintf("edge pass 4 delta=%f", delta))
			}
			{
				const n = 200
				const delta = 0.1400
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5-delta, 0.5-delta, 0, 0.5-delta, 0.5-delta, 1, fmt.Sprintf("edge pass 4 delta=%f", delta))
			}
			{
				const n = 200
				const delta = 0.1401
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5-delta, 0.5-delta, 0, 0.5-delta, 0.5-delta, 1, fmt.Sprintf("edge pass 4 delta=%f", delta))
			}
		}

		{
			const n = 200
			const dx = -0.0135
			s := makeSphere(n)
			s.pass(0, 0, 0, 0, 0, 1, "pass 1")
			s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
			s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
			s.pass(0.5+dx, 1, 1, 0.5+dx, -1, -1, fmt.Sprintf("diagonal face pass 4 dx=%f", dx))
		}

		if false {
			{
				const n = 200
				const dx = -0.0134
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5+dx, 1, 1, 0.5+dx, -1, -1, fmt.Sprintf("diagonal face pass 4 dx=%f", dx))
			}

			{
				const n = 200
				const dx = -0.0135
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5+dx, 1, 1, 0.5+dx, -1, -1, fmt.Sprintf("diagonal face pass 4 dx=%f", dx))
			}

			{
				const n = 200
				const dx = -0.0136
				s := makeSphere(n)
				s.pass(0, 0, 0, 0, 0, 1, "pass 1")
				s.pass(0, 0, 0, 0, 1, 0, "perpendicular centered pass 2")
				s.pass(0, 0, 0, 1, 0, 0, "perpendicular centered pass 3")
				s.pass(0.5+dx, 1, 1, 0.5+dx, -1, -1, fmt.Sprintf("diagonal face pass 4 dx=%f", dx))
			}
		}
	}
}
