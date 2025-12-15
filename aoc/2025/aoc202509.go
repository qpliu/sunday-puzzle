package main

import (
	"cmp"
	"math"
	"slices"
)

func init() {
	Register(&aoc202509{
		AOC: AOC{
			Day: 9,
			Tests: []Test{
				Test{
					Input: `7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3
`,
					Part1: "50",
					Part2: "24",
				},
			},
		},
	})
}

type aoc202509 struct {
	AOC
}

func (aoc *aoc202509) parse(input *Input) (int, int, int, int, [][2]int) {
	xmin := math.MaxInt
	xmax := 0
	ymin := math.MaxInt
	ymax := 0
	var tiles [][2]int
	for {
		x, _ := input.Int()
		y, ok := input.Int()
		if !ok {
			return xmin, xmax, ymin, ymax, tiles
		}
		xmin = min(xmin, x)
		xmax = max(xmax, x)
		ymin = min(ymin, y)
		ymax = max(ymax, y)
		tiles = append(tiles, [2]int{x, y})
	}
}

func (aoc *aoc202509) nearestToCorner(xc, yc int, tiles [][2]int) [][3]int {
	nearest := make([][3]int, len(tiles))
	for i, tile := range tiles {
		dx := tile[0] - xc
		dy := tile[1] - yc
		nearest[i] = [3]int{dx*dx + dy*dy, tile[0], tile[1]}
	}
	slices.SortFunc(nearest, func(a, b [3]int) int {
		return cmp.Compare(a[0], b[0])
	})
	return nearest
}

func (aoc *aoc202509) Part1(input *Input) string {
	xmin, xmax, ymin, ymax, tiles := aoc.parse(input)
	upperLeft := aoc.nearestToCorner(xmin, ymin, tiles)
	upperRight := aoc.nearestToCorner(xmax, ymin, tiles)
	lowerLeft := aoc.nearestToCorner(xmin, ymax, tiles)
	lowerRight := aoc.nearestToCorner(xmax, ymax, tiles)

	result := 0
	heuristic := min(len(tiles), 5)
	for _, t1 := range upperLeft[:heuristic] {
		for _, t2 := range lowerRight[:heuristic] {
			result = max(result, (max(t1[1]-t2[1], t2[1]-t1[1])+1)*(max(t1[2]-t2[2], t2[2]-t1[2])+1))
		}
	}
	for _, t1 := range lowerLeft[:heuristic] {
		for _, t2 := range upperRight[:heuristic] {
			result = max(result, (max(t1[1]-t2[1], t2[1]-t1[1])+1)*(max(t1[2]-t2[2], t2[2]-t1[2])+1))
		}
	}
	return IntResult(result)
}

func (aoc *aoc202509) makeHVsegs(tiles [][2]int) ([][3]int, [][3]int) {
	hsegs := make([][3]int, len(tiles)/2)
	vsegs := make([][3]int, len(tiles)/2)
	if tiles[0][0] == tiles[1][0] {
		for i := range len(tiles) / 2 {
			j := 2 * i
			k := j + 1
			l := (k + 1) % len(tiles)
			vsegs[i] = [3]int{
				tiles[j][0],
				min(tiles[j][1], tiles[k][1]),
				max(tiles[j][1], tiles[k][1]),
			}
			hsegs[i] = [3]int{
				tiles[k][1],
				min(tiles[k][0], tiles[l][0]),
				max(tiles[k][0], tiles[l][0]),
			}
		}
	} else {
		for i := range len(tiles) / 2 {
			j := 2 * i
			k := j + 1
			l := (k + 1) % len(tiles)
			hsegs[i] = [3]int{
				tiles[j][1],
				min(tiles[j][0], tiles[k][0]),
				max(tiles[j][0], tiles[k][0]),
			}
			vsegs[i] = [3]int{
				tiles[k][0],
				min(tiles[k][1], tiles[l][1]),
				max(tiles[k][1], tiles[l][1]),
			}
		}
	}
	slices.SortFunc(hsegs, func(a, b [3]int) int {
		return cmp.Compare(a[0], b[0])
	})
	slices.SortFunc(vsegs, func(a, b [3]int) int {
		return cmp.Compare(a[0], b[0])
	})
	return hsegs, vsegs
}

func (aoc *aoc202509) sortedAnnotatedTiles(tiles [][2]int, hsegs, vsegs [][3]int) [][5]int {
	ts := make([][5]int, len(tiles))
	for i, t := range tiles {
		t1 := tiles[(i+len(tiles)-1)%len(tiles)]
		t2 := tiles[(i+1)%len(tiles)]
		ts[i] = [5]int{
			max(t[0]-t1[0], t1[0]-t[0],
				t[0]-t2[0], t2[0]-t[0],
				t[1]-t1[1], t1[1]-t[1],
				t[1]-t2[1], t2[1]-t[1]),
			t[0],
			t[1],
			aoc.getIndex(t[0], vsegs),
			aoc.getIndex(t[1], hsegs),
		}
	}
	slices.SortFunc(ts, func(a, b [5]int) int {
		return cmp.Compare(b[0], a[0]) // descending sort
	})
	return ts
}

func (aoc *aoc202509) getIndex(x int, segs [][3]int) int {
	lo := 0
	hi := len(segs) - 1
	for {
		mid := (lo + hi) / 2
		switch cmp.Compare(x, segs[mid][0]) {
		case 0:
			return mid
		case -1:
			hi = mid
		case 1:
			if mid == lo {
				return hi
			}
			lo = mid
		}
	}
}

func (aoc *aoc202509) Part2(input *Input) string {
	_, _, _, _, tiles := aoc.parse(input)
	hsegs, vsegs := aoc.makeHVsegs(tiles)
	ts := aoc.sortedAnnotatedTiles(tiles, hsegs, vsegs)

	result := 0
	// based on the shape of the input, one of the corners must be
	// one of the four at an end of the one of two longest segments
	heuristic := min(4, len(ts))
	for i, t := range ts[:heuristic] {
		// based on the shape of the input, can skip concavity
		// checks for pairs of corners (which would be needed
		// for perverse shapes like a crescent)
		// and can ignore the possibility of adjacent parallel
		// segments
	t2loop:
		for _, t2 := range ts[i+1:] {
			xlo := min(t[1], t2[1])
			xhi := max(t[1], t2[1])
			iseglo := min(t[4], t2[4]) + 1
			iseghi := max(t[4], t2[4])
			if iseglo <= iseghi {
				for _, hseg := range hsegs[iseglo:iseghi] {
					if hseg[1] < xhi && hseg[2] > xlo {
						continue t2loop
					}
				}
			}
			ylo := min(t[2], t2[2])
			yhi := max(t[2], t2[2])
			iseglo = min(t[3], t2[3]) + 1
			iseghi = max(t[3], t2[3])
			if iseglo <= iseghi {
				for _, vseg := range vsegs[iseglo:iseghi] {
					if vseg[1] < yhi && vseg[2] > ylo {
						continue t2loop
					}
				}
			}
			result = max(result, (1+xhi-xlo)*(1+yhi-ylo))
		}
	}
	return IntResult(result)
}
