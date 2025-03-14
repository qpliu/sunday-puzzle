package main

import (
	"math"
)

func dist(x, y float64) float64 {
	dr := 1 - math.Sqrt(x*x + y*y)
	return min(dr, math.Abs(y))
}

func integrate(n int) float64 {
	npoints := 0
	sum := float64(0)
	for ix := range n {
		x := float64(ix)/float64(n)
		for iy := range n {
			y := float64(iy)/float64(n)
			if x*x + y*y > 1 {
				break
			}
			npoints++
			sum += dist(x, y)
		}
	}
	return sum/float64(npoints)
}

func main() {
	for _, n := range []int{10, 100, 1000, 10000} {
		println(n, integrate(n))
	}
}
