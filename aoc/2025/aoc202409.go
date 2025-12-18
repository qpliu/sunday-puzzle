package main

import (
	"container/list"
)

func init() {
	Register(&aoc202409{
		AOC: AOC{
			Day:           9,
			InputFilename: "../2024/input/09.txt",
			Tests: []Test{
				Test{
					Input: `2333133121414131402
`,
					Part1: "1928",
					Part2: "2858",
				},
			},
		},
	})
}

type aoc202409 struct {
	AOC
}

func (aoc *aoc202409) parse(input *Input) *list.List {
	disk := list.New()
	id := 0
	for {
		file, ok := input.Char()
		if !ok {
			return disk
		}
		free, _ := input.Char()
		if free < '0' || free > '9' {
			free = '0'
		}
		disk.PushBack([3]int{id, int(file - '0'), int(free - '0')})
		id++
	}
}

func (aoc *aoc202409) checksum(disk *list.List) int {
	sum := 0
	i := 0
	for e := disk.Front(); e != nil; e = e.Next() {
		v := e.Value.([3]int)
		sum += v[0] * (i*v[1] + v[1]*(v[1]-1)/2)
		i += v[1] + v[2]
	}
	return sum
}

func (aoc *aoc202409) compact(disk *list.List) {
	f := disk.Front()
	b := disk.Back()
	for {
		if f == b {
			return
		}
		fv := f.Value.([3]int)
		if fv[2] == 0 {
			f = f.Next()
			continue
		}
		bv := b.Value.([3]int)
		if fv[2] >= bv[1] {
			f.Value = [3]int{fv[0], fv[1], 0}
			b.Value = [3]int{bv[0], bv[1], fv[2] - bv[1]}
			nextb := b.Prev()
			disk.MoveAfter(b, f)
			f = b
			b = nextb
		} else {
			f.Value = [3]int{fv[0], fv[1], 0}
			b.Value = [3]int{bv[0], bv[1] - fv[2], bv[2] + fv[2]}
			f = disk.InsertAfter([3]int{bv[0], fv[2], 0}, f).Next()
		}
	}
}

func (aoc *aoc202409) Part1(input *Input) string {
	disk := aoc.parse(input)
	aoc.compact(disk)
	return IntResult(aoc.checksum(disk))
}

func (aoc *aoc202409) compact2(disk *list.List) {
	f := disk.Front()
	target := disk.Back()
	for {
		if f == target {
			return
		}
		fv := f.Value.([3]int)
		if fv[2] == 0 {
			f = f.Next()
			continue
		}
		tv := target.Value.([3]int)
		nextTarget := target.Prev()
		dest := f
		for {
			dv := dest.Value.([3]int)
			if dv[2] < tv[1] {
				dest = dest.Next()
				if dest == target {
					break
				}
				continue
			}
			dest.Value = [3]int{dv[0], dv[1], 0}
			if dest == nextTarget {
				target.Value = [3]int{tv[0], tv[1], tv[2] + dv[2]}
			} else {
				nv := nextTarget.Value.([3]int)
				nextTarget.Value = [3]int{nv[0], nv[1], nv[2] + tv[1] + tv[2]}
				target.Value = [3]int{tv[0], tv[1], dv[2] - tv[1]}
				disk.MoveAfter(target, dest)
			}
			break
		}
		target = nextTarget
	}
}

func (aoc *aoc202409) Part2(input *Input) string {
	disk := aoc.parse(input)
	aoc.compact2(disk)
	return IntResult(aoc.checksum(disk))
}
