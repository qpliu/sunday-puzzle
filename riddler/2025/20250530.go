package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

// Count the number of times a random strand passes within a
// square with side length 2*epsilon centered at the center
// or a corner or the middle of an edge of a unit square while
// still within the unit square.

// To normalize based on the sizes of the epsilon squares,
// let the weighting of the center be 1.
// Let the weightings of the corners be 1 so that
// the ratio of the center to one corner is the count of the center
// to the counts of all the corners.
// Let the weightings of the middles of the edges be 1/2
// so that the ratio of the center to the middle of one edge is
// the 2 times the count of the center to the count of the middles
// of all the edges.

// A single strand can pass near up to two corners, up to two
// middles of edges, as well as the center.

func passesNear(xtarget, ytarget, x1, y1, x2, y2, epsilon float64) bool {
	xa := max(xtarget-epsilon, 0)
	ya := max(ytarget-epsilon, 0)
	xb := min(xtarget+epsilon, 1)
	yb := min(ytarget+epsilon, 1)
	if x1 == x2 {
		return xa < x1 && x1 < xb
	}
	if y1 == y2 {
		return ya < y1 && y1 < yb
	}
	// (x - x1)/(x2-x2) = (y-y1)/(y2-y1)
	y := y1 + (xa-x1)*(y2-y1)/(x2-x1)
	if ya < y && y < yb {
		return true
	}
	y = y1 + (xb-x1)*(y2-y1)/(x2-x1)
	if ya < y && y < yb {
		return true
	}
	x := x1 + (ya-y1)*(x2-x1)/(y2-y1)
	if xa < x && x < xb {
		return true
	}
	x = x1 + (yb-y1)*(x2-x1)/(y2-y1)
	if xa < x && x < xb {
		return true
	}
	return false
}

func counts(x1, y1, x2, y2, epsilon float64) (int, int, int) {
	center := 0
	if passesNear(0.5, 0.5, x1, y1, x2, y2, epsilon) {
		center++
	}
	corners := 0
	if passesNear(0, 0, x1, y1, x2, y2, epsilon) {
		corners++
	}
	if passesNear(0, 1, x1, y1, x2, y2, epsilon) {
		corners++
	}
	if passesNear(1, 0, x1, y1, x2, y2, epsilon) {
		corners++
	}
	if passesNear(1, 1, x1, y1, x2, y2, epsilon) {
		corners++
	}
	mids := 0
	if passesNear(0.5, 0, x1, y1, x2, y2, epsilon) {
		mids++
	}
	if passesNear(0.5, 1, x1, y1, x2, y2, epsilon) {
		mids++
	}
	if passesNear(0, 0.5, x1, y1, x2, y2, epsilon) {
		mids++
	}
	if passesNear(1, 0.5, x1, y1, x2, y2, epsilon) {
		mids++
	}
	return center, corners, mids
}

func trial(epsilon float64, r *rand.Rand) (int, int, int) {
	return counts(r.Float64(), r.Float64(), r.Float64(), r.Float64(), epsilon)
}

func simulate(ntrials, ncpu int, epsilon float64, r *rand.Rand) {
	ch := make(chan [3]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			counts := [3]int{0, 0, 0}
			for _ = range ntrials {
				c0, c1, c2 := trial(epsilon, r)
				counts[0] += c0
				counts[1] += c1
				counts[2] += c2
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	counts := [3]int{0, 0, 0}
	for _ = range ncpu {
		c := <-ch
		counts[0] += c[0]
		counts[1] += c[1]
		counts[2] += c[2]
	}
	if counts[1] == 0 || counts[2] == 0 {
		fmt.Printf("%d/%d/%d\n", counts[0], counts[1], counts[2])
	} else {
		fmt.Printf("%d/%d/%d %f %f\n", counts[0], counts[1], counts[2], float64(counts[0])/float64(counts[1]), float64(2*counts[0])/float64(counts[2]))
	}
}

func main() {
	const seed = 20250530
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	{
		const ntrials = 1000000
		const epsilon = 0.0001
		simulate(ntrials, ncpu, epsilon, r)
	}

	{
		const ntrials = 1000000000
		const epsilon = 0.0000001
		simulate(ntrials, ncpu, epsilon, r)
	}
}
