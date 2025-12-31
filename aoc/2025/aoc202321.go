package main

import (
	"math/big"
	"math/bits"
	"sync"
)

func init() {
	Register(&aoc202321{
		AOC: AOC{
			Day:           21,
			InputFilename: "../2023/input/21.txt",
			Tests: []Test{
				Test{
					Input: `...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
`,
					Part1: "16",
					Part2: "",
				},
			},
		},
	})
}

type aoc202321 struct {
	AOC
}

func (aoc *aoc202321) parse1(input *Input) ([]*big.Int, []*big.Int) {
	grid := []*big.Int{}
	walk := []*big.Int{}
	x := 0
	var gridRow, walkRow *big.Int
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		if x == 0 {
			gridRow = &big.Int{}
			gridRow.SetBit(gridRow, 0, 1)
			grid = append(grid, gridRow)
			walkRow = &big.Int{}
			walk = append(walk, walkRow)
			x++
		}
		switch ch {
		case '\n':
			gridRow.SetBit(gridRow, x, 1)
			x = 0
		case '#':
			gridRow.SetBit(gridRow, x, 1)
			x++
		case 'S':
			walkRow.SetBit(walkRow, x, 1)
			x++
		default:
			x++
		}
	}
	return grid, walk
}

func (aoc *aoc202321) part1(nsteps int, input *Input) string {
	grid, walk := aoc.parse1(input)

	tmp := &big.Int{}
	tmpWalk := []*big.Int{}
	n := len(walk)
	for range n {
		tmpWalk = append(tmpWalk, &big.Int{})
	}

	for range nsteps {
		thisRow := walk[0]
		{
			nextRow := walk[1]
			r := tmpWalk[0]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, nextRow)
			r.AndNot(r, grid[0])
			thisRow = nextRow
		}
		lastRow := walk[0]
		for i := range n - 2 {
			nextRow := walk[i+2]
			r := tmpWalk[i+1]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, lastRow)
			r.Or(r, nextRow)
			r.AndNot(r, grid[i+1])
			lastRow, thisRow = thisRow, nextRow
		}
		{
			r := tmpWalk[n-1]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, lastRow)
			r.AndNot(r, grid[n-1])
		}
		walk, tmpWalk = tmpWalk, walk
	}

	result := 0
	for _, row := range walk {
		for _, b := range row.Bytes() {
			result += bits.OnesCount8(b)
		}
	}
	return IntResult(result)
}

func (aoc *aoc202321) Test1(input *Input) string {
	return aoc.part1(6, input)
}

func (aoc *aoc202321) Part1(input *Input) string {
	return aoc.part1(64, input)
}

func (aoc *aoc202321) parse2(input *Input) (int, int, []*big.Int) {
	grid := []*big.Int{}
	xstart, ystart, width := 0, 0, 0
	x, y := 0, 0
	var gridRow *big.Int
	for ch, ok := input.Char(); ok; ch, ok = input.Char() {
		if x == 0 {
			gridRow = &big.Int{}
			gridRow.SetBit(gridRow, 0, 1)
			grid = append(grid, gridRow)
			x++
		}
		switch ch {
		case '\n':
			gridRow.SetBit(gridRow, x, 1)
			if width == 0 {
				width = x - 1
			} else if width != x-1 {
				panic("bad input: nonuniform width")
			}
			x = 0
			y++
		case '#':
			gridRow.SetBit(gridRow, x, 1)
			x++
		case 'S':
			if x != y+1 || width != 2*x-1 {
				panic("bad input: start not at center")
			}
			xstart, ystart = x, y
			x++
		default:
			x++
		}
	}
	if len(grid) != width {
		panic("bad input: not square")
	}
	return xstart, ystart, grid
}

func (aoc *aoc202321) reachCounts(xstart, ystart int, grid []*big.Int) []int {
	tmp := &big.Int{}
	walk, tmpWalk := []*big.Int{}, []*big.Int{}
	n := len(grid)
	for y := range n {
		r := &big.Int{}
		if y == ystart {
			r.SetBit(r, xstart, 1)
		}
		walk = append(walk, r)
		tmpWalk = append(tmpWalk, &big.Int{})
	}

	prevCount2, prevCount := 0, 0
	counts := []int{}
	for {
		count := 0
		for _, row := range walk {
			for _, b := range row.Bytes() {
				count += bits.OnesCount8(b)
			}
		}
		if count == prevCount2 {
			return counts
		}
		counts = append(counts, count)
		prevCount2, prevCount = prevCount, count

		thisRow := walk[0]
		{
			nextRow := walk[1]
			r := tmpWalk[0]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, nextRow)
			r.AndNot(r, grid[0])
			thisRow = nextRow
		}
		lastRow := walk[0]
		for i := range n - 2 {
			nextRow := walk[i+2]
			r := tmpWalk[i+1]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, lastRow)
			r.Or(r, nextRow)
			r.AndNot(r, grid[i+1])
			lastRow, thisRow = thisRow, nextRow
		}
		{
			r := tmpWalk[n-1]
			r.Lsh(thisRow, 1)
			tmp.Rsh(thisRow, 1)
			r.Or(r, tmp)
			r.Or(r, lastRow)
			r.AndNot(r, grid[n-1])
		}
		walk, tmpWalk = tmpWalk, walk
	}
}

func (aoc *aoc202321) vstack(grid []*big.Int) []*big.Int {
	grid2h := []*big.Int{}
	for range 2 {
		for _, row := range grid {
			grid2h = append(grid2h, (&big.Int{}).Set(row))
		}
	}
	return grid2h
}

func (aoc *aoc202321) hstack(grid []*big.Int) []*big.Int {
	tmp := &big.Int{}
	grid2w := []*big.Int{}
	w := len(grid)
	for _, row := range grid {
		tmp.Rsh(row, 1)
		tmp.Lsh(tmp, uint(w)+1)
		r := &big.Int{}
		r.SetBit(row, w+1, 0)
		grid2w = append(grid2w, r.Or(r, tmp))
	}
	return grid2w
}

func (aoc *aoc202321) strip(xstart, ystart, stepsToNext, nsteps int, grid []*big.Int) int {
	counts := aoc.reachCounts(xstart, ystart, grid)
	fullSteps := len(counts) + 1
	fullCount := counts[len(counts)-1]
	fullyReachable := (nsteps-fullSteps)/stepsToNext + 1
	reachable := nsteps/stepsToNext + 1
	result := fullyReachable * fullCount
	for n := range reachable - fullyReachable {
		result += counts[nsteps-(n+fullyReachable)*stepsToNext]
	}
	return result
}

func (aoc *aoc202321) corner(xstart, ystart, stepsToNext, nsteps int, grid []*big.Int) int {
	counts := aoc.reachCounts(xstart, ystart, grid)
	fullSteps := len(counts) + 1
	fullCount := counts[len(counts)-1]
	fullyReachable := (nsteps-fullSteps)/stepsToNext + 1
	reachable := nsteps/stepsToNext + 1
	result := fullyReachable * (fullyReachable + 1) * fullCount / 2
	for n := range reachable - fullyReachable {
		result += counts[nsteps-(n+fullyReachable)*stepsToNext] * (fullyReachable + 1 + n)
	}
	return result
}

func (aoc *aoc202321) Part2(input *Input) string {
	const nsteps = 26501365
	xstart, ystart, grid := aoc.parse2(input)
	width := len(grid)

	wg := &sync.WaitGroup{}
	centerCount := 0
	wg.Add(1)
	go func() {
		centerCount = 0
		counts := aoc.reachCounts(xstart, ystart, grid)
		if len(counts)%2 == nsteps%2 {
			// first element of counts is zero steps
			centerCount = counts[len(counts)-2]
		} else {
			centerCount = counts[len(counts)-1]
		}
		wg.Done()
	}()

	grid2h := aoc.vstack(grid)

	northCount := 0
	wg.Add(1)
	go func() {
		northCount = aoc.strip(xstart, 2*width-1, 2*width, nsteps-(ystart+1), grid2h)
		wg.Done()
	}()

	southCount := 0
	wg.Add(1)
	go func() {
		southCount = aoc.strip(xstart, 0, 2*width, nsteps-(ystart+1), grid2h)
		wg.Done()
	}()

	grid2w := aoc.hstack(grid)

	eastCount := 0
	wg.Add(1)
	go func() {
		eastCount = aoc.strip(1, ystart, 2*width, nsteps-(ystart+1), grid2w)
		wg.Done()
	}()

	westCount := 0
	wg.Add(1)
	go func() {
		westCount = aoc.strip(2*width, ystart, 2*width, nsteps-(ystart+1), grid2w)
		wg.Done()
	}()

	grid4x := aoc.vstack(grid2w)

	northwestCount := 0
	wg.Add(1)
	go func() {
		northwestCount = aoc.corner(2*width, 2*width-1, 2*width, nsteps-2*(ystart+1), grid4x)
		wg.Done()
	}()

	northeastCount := 0
	wg.Add(1)
	go func() {
		northeastCount = aoc.corner(1, 2*width-1, 2*width, nsteps-2*(ystart+1), grid4x)
		wg.Done()
	}()

	southwestCount := 0
	wg.Add(1)
	go func() {
		southwestCount = aoc.corner(2*width, 0, 2*width, nsteps-2*(ystart+1), grid4x)
		wg.Done()
	}()

	southeastCount := 0
	wg.Add(1)
	go func() {
		southeastCount = aoc.corner(1, 0, 2*width, nsteps-2*(ystart+1), grid4x)
		wg.Done()
	}()

	wg.Wait()
	return IntResult(centerCount + northCount + southCount + eastCount + westCount + northwestCount + northeastCount + southwestCount + southeastCount)
}
