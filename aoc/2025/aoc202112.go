package main

import (
	"strings"
)

func init() {
	Register(&aoc202112{
		AOC: AOC{
			Day:           12,
			InputFilename: "../2021/input/12.txt",
			Tests: []Test{
				Test{
					Input: `start-A
start-b
A-c
A-b
b-d
A-end
b-end
`,
					Part1: "10",
					Part2: "36",
				},
				Test{
					Input: `dc-end
HN-start
start-kj
dc-start
dc-HN
LN-dc
HN-end
kj-sa
kj-HN
kj-dc
`,
					Part1: "19",
					Part2: "103",
				},
				Test{
					Input: `fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW
`,
					Part1: "226",
					Part2: "3509",
				},
			},
		},
	})
}

type aoc202112 struct {
	AOC
}

func (aoc *aoc202112) parse(input *Input) (int, int, []bool, [][]int) {
	start, end := -1, -1
	big := []bool{}
	exits := [][]int{}
	names := map[string]int{}
	for line := range input.Lines() {
		dash := strings.Index(line, "-")
		name1 := line[:dash]
		name2 := line[dash+1:]
		index1, ok := names[name1]
		if !ok {
			index1 = len(big)
			names[name1] = index1
			big = append(big, name1[0] <= 'Z')
			exits = append(exits, []int{})
			if name1 == "start" {
				start = index1
			} else if name1 == "end" {
				end = index1
			}
		}
		index2, ok := names[name2]
		if !ok {
			index2 = len(big)
			names[name2] = index2
			big = append(big, name2[0] <= 'Z')
			exits = append(exits, []int{})
			if name2 == "start" {
				start = index2
			} else if name2 == "end" {
				end = index2
			}
		}
		if index1 != end && index2 != start {
			exits[index1] = append(exits[index1], index2)
		}
		if index2 != end && index1 != start {
			exits[index2] = append(exits[index2], index1)
		}
	}
	return start, end, big, exits
}

// There are no direct connections between big caves in the any of the
// examples, nor in my input, so there is no need to worry about getting
// stuck in loops.

func (aoc *aoc202112) Part1(input *Input) string {
	start, end, big, exits := aoc.parse(input)

	type path struct {
		loc, visited int
	}

	queue := NewQueue[path]()
	queue.Enqueue(path{start, 1 << start})
	paths := 0
	for !queue.Empty() {
		p := queue.Dequeue()
		for _, exit := range exits[p.loc] {
			if exit == end {
				paths++
			} else if big[exit] || p.visited&(1<<exit) == 0 {
				queue.Enqueue(path{exit, p.visited | (1 << exit)})
			}
		}
	}
	return IntResult(paths)
}

func (aoc *aoc202112) Part2(input *Input) string {
	start, end, big, exits := aoc.parse(input)

	type path struct {
		loc, visited int
		visitedTwice bool
	}

	queue := NewQueue[path]()
	queue.Enqueue(path{start, 1 << start, false})
	paths := 0
	for !queue.Empty() {
		p := queue.Dequeue()
		for _, exit := range exits[p.loc] {
			if exit == end {
				paths++
			} else if big[exit] {
				queue.Enqueue(path{exit, p.visited, p.visitedTwice})
			} else if p.visited&(1<<exit) == 0 {
				queue.Enqueue(path{exit, p.visited | (1 << exit), p.visitedTwice})
			} else if !p.visitedTwice {
				queue.Enqueue(path{exit, p.visited, true})
			}
		}
	}
	return IntResult(paths)
}
