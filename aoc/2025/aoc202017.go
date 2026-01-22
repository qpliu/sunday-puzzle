package main

import (
	"math/bits"
	"runtime"
	"sync"
)

func init() {
	Register(&aoc202017{
		AOC: AOC{
			Day:           17,
			InputFilename: "../2020/input/17.txt",
			Tests: []Test{
				Test{
					Input: `.#.
..#
###
`,
					Part1: "112",
					Part2: "848",
				},
			},
		},
	})
}

type aoc202017 struct {
	AOC
}

func (aoc *aoc202017) parse(input *Input) []uint64 {
	x := 0
	y := 0
	cells := []uint64{0}
	for ch := range input.Chars() {
		switch ch {
		case '\n':
			x = 0
			y++
			cells = append(cells, 0)
		case '#':
			cells[y] |= 1 << x
			x++
		default:
			x++
		}
	}
	return cells
}

func (aoc *aoc202017) Part1(input *Input) string {
	cells := [][]uint64{aoc.parse(input)}
	var newCells [][]uint64
	nz := 1
	ny := len(cells[0])
	var wg sync.WaitGroup
	in := make(chan [2]int)
	for range runtime.NumCPU() {
		go func() {
			for zy := range in {
				z, y := zy[0], zy[1]
				sum0 := uint64(0)
				sum1 := uint64(0)
				overflow := uint64(0)
				for _, dz := range [...]int{-2, -1, 0} {
					if z+dz < 0 || z+dz >= nz {
						continue
					}
					for _, dy := range [...]int{-2, -1, 0} {
						if y+dy < 0 || y+dy >= ny {
							continue
						}
						c := cells[z+dz][y+dy]

						carry := sum0 & c
						sum0 ^= c
						overflow |= sum1 & carry
						sum1 ^= carry

						c <<= 1
						if dy != -1 || dz != -1 {
							carry = sum0 & c
							sum0 ^= c
							overflow |= sum1 & carry
							sum1 ^= carry
						}

						c <<= 1
						carry = sum0 & c
						sum0 ^= c
						overflow |= sum1 & carry
						sum1 ^= carry
					}
				}
				c := uint64(0)
				if z > 0 && z <= nz && y > 0 && y <= ny {
					c = cells[z-1][y-1] << 1
				}
				newCells[z][y] = ((c | sum0) & sum1) &^ overflow
				wg.Done()
			}
		}()
	}
	for range 6 {
		newCells = make([][]uint64, 2+nz)
		wg.Add((2 + nz) * (2 + ny))
		for z := range 2 + nz {
			newCells[z] = make([]uint64, 2+ny)
			for y := range 2 + ny {
				in <- [2]int{z, y}
			}
		}
		wg.Wait()
		cells = newCells
		ny += 2
		nz += 2
	}
	close(in)
	result := 0
	for _, cellsZ := range cells {
		for _, cellsZY := range cellsZ {
			result += bits.OnesCount64(cellsZY)
		}
	}
	return IntResult(result)
}

func (aoc *aoc202017) Part2(input *Input) string {
	cells := [][][]uint64{[][]uint64{aoc.parse(input)}}
	var newCells [][][]uint64
	nw := 1
	nz := 1
	ny := len(cells[0][0])
	var wg sync.WaitGroup
	in := make(chan [3]int)
	for range runtime.NumCPU() {
		go func() {
			for wzy := range in {
				w, z, y := wzy[0], wzy[1], wzy[2]
				sum0 := uint64(0)
				sum1 := uint64(0)
				overflow := uint64(0)
				for _, dw := range [...]int{-2, -1, 0} {
					if w+dw < 0 || w+dw >= nw {
						continue
					}
					for _, dz := range [...]int{-2, -1, 0} {
						if z+dz < 0 || z+dz >= nz {
							continue
						}
						for _, dy := range [...]int{-2, -1, 0} {
							if y+dy < 0 || y+dy >= ny {
								continue
							}
							c := cells[w+dw][z+dz][y+dy]

							carry := sum0 & c
							sum0 ^= c
							overflow |= sum1 & carry
							sum1 ^= carry

							c <<= 1
							if dy != -1 || dz != -1 || dw != -1 {
								carry = sum0 & c
								sum0 ^= c
								overflow |= sum1 & carry
								sum1 ^= carry
							}

							c <<= 1
							carry = sum0 & c
							sum0 ^= c
							overflow |= sum1 & carry
							sum1 ^= carry
						}
					}
				}
				c := uint64(0)
				if w > 0 && w <= nw && z > 0 && z <= nz && y > 0 && y <= ny {
					c = cells[w-1][z-1][y-1] << 1
				}
				newCells[w][z][y] = ((c | sum0) & sum1) &^ overflow
				wg.Done()
			}
		}()
	}
	for range 6 {
		newCells = make([][][]uint64, 2+nw)
		wg.Add((2 + nw) * (2 + nz) * (2 + ny))
		for w := range 2 + nw {
			newCells[w] = make([][]uint64, 2+nz)
			for z := range 2 + nz {
				newCells[w][z] = make([]uint64, 2+ny)
				for y := range 2 + ny {
					in <- [3]int{w, z, y}
				}
			}
		}
		wg.Wait()
		cells = newCells
		ny += 2
		nz += 2
		nw += 2
	}
	close(in)
	result := 0
	for _, cellsW := range cells {
		for _, cellsWZ := range cellsW {
			for _, cellsWZY := range cellsWZ {
				result += bits.OnesCount64(cellsWZY)
			}
		}
	}
	return IntResult(result)
}
