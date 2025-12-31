package main

import (
	"runtime"
)

func init() {
	Register(&aoc202323{
		AOC: AOC{
			Day:           23,
			InputFilename: "../2023/input/23.txt",
			Tests: []Test{
				Test{
					Input: `#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
`,
					Part1: "94",
					Part2: "154",
				},
			},
		},
	})
}

type aoc202323 struct {
	AOC
}

func (aoc *aoc202323) parse(part1 bool, input *Input) [][][2]int {
	w, h, grid := input.Grid()

	startX := 0
	endX := 0
	for x := range w {
		if grid[XY{x, 0}] == '.' {
			startX = x
		}
		if grid[XY{x, h - 1}] == '.' {
			endX = x
		}
	}

	type node struct {
		index     int
		xy        XY
		neighbors [4]*node
		steps     [4]int
	}
	nodeList := []*node{
		&node{
			index: 0,
			xy:    XY{startX, 0},
		},
		&node{
			index: 1,
			xy:    XY{endX, h - 1},
		},
	}

	queue := NewQueue[*node]()
	nodeByXY := map[XY]*node{}
	for _, n := range nodeList {
		nodeByXY[n.xy] = n
		queue.Enqueue(n)
	}

	for !queue.Empty() {
		n := queue.Dequeue()
		for i, dir := range [...]int{DirR, DirD, DirL, DirU} {
			nsteps := 0
			xy := AdvanceXY(n.xy, dir, 1)
			switch grid[xy] {
			case '^':
				if !part1 || dir == DirU {
					nsteps++
				}
			case 'v':
				if !part1 || dir == DirD {
					nsteps++
				}
			case '<':
				if !part1 || dir == DirL {
					nsteps++
				}
			case '>':
				if !part1 || dir == DirR {
					nsteps++
				}
			case '.':
				nsteps++
			}
			if nsteps == 0 {
				continue
			}
			for {
				destnode := nodeByXY[xy]
				if destnode != nil {
					n.neighbors[i] = destnode
					n.steps[i] = nsteps
					break
				}
				nportals := 0
				nexits := 0
				nextDir := dir
				for _, ndir := range [...]int{dir, TurnL(dir), TurnR(dir)} {
					switch grid[AdvanceXY(xy, ndir, 1)] {
					case '^':
						nportals++
						if !part1 || ndir == DirU {
							nexits++
							nextDir = ndir
						}
					case 'v':
						nportals++
						if !part1 || ndir == DirD {
							nexits++
							nextDir = ndir
						}
					case '<':
						nportals++
						if !part1 || ndir == DirL {
							nexits++
							nextDir = ndir
						}
					case '>':
						nportals++
						if !part1 || ndir == DirR {
							nexits++
							nextDir = ndir
						}
					case '.':
						nportals++
						nexits++
						nextDir = ndir
					}
				}
				if nexits == 0 {
					break
				}
				if nportals == 1 {
					dir = nextDir
					xy = AdvanceXY(xy, dir, 1)
					nsteps++
					continue
				}
				destnode = nodeByXY[xy]
				if destnode == nil {
					destnode = &node{
						index: len(nodeList),
						xy:    xy,
					}
					nodeList = append(nodeList, destnode)
					nodeByXY[xy] = destnode
					queue.Enqueue(destnode)
				}
				n.neighbors[i] = destnode
				n.steps[i] = nsteps
				break
			}
		}
	}

	graph := [][][2]int{}
	for _, n := range nodeList {
		edges := [][2]int{}
		for i, neighbor := range n.neighbors {
			if neighbor != nil {
				edges = append(edges, [2]int{neighbor.index, n.steps[i]})
			}
		}
		graph = append(graph, edges)
	}
	return graph
}

func (aoc *aoc202323) result(graph [][][2]int) int {
	type walk struct {
		loc     int
		covered BitSet64
		steps   int
	}

	queue := NewQueue[walk]()
	queue.Enqueue(walk{0, BitSet64(0).Add(0), 0})

	result := 0
	for queue.Len() > 0 && queue.Len() < 4*runtime.NumCPU() {
		w := queue.Dequeue()
		if w.loc == 1 {
			result = max(result, w.steps)
			continue
		}
		for _, dest := range graph[w.loc] {
			if !w.covered.Contains(dest[0]) {
				queue.Enqueue(walk{
					loc:     dest[0],
					covered: w.covered.Add(dest[0]),
					steps:   w.steps + dest[1],
				})
			}
		}
	}

	in := make(chan walk)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			res := result
			q := NewQueue[walk]()
			for w := range in {
				q.Enqueue(w)
				for !q.Empty() {
					w := q.Dequeue()
					if w.loc == 1 {
						res = max(res, w.steps)
						continue
					}
					for _, dest := range graph[w.loc] {
						if !w.covered.Contains(dest[0]) {
							q.Enqueue(walk{
								loc:     dest[0],
								covered: w.covered.Add(dest[0]),
								steps:   w.steps + dest[1],
							})
						}
					}
				}
			}
			out <- res
		}()
	}

	for !queue.Empty() {
		in <- queue.Dequeue()
	}
	close(in)

	for range runtime.NumCPU() {
		result = max(result, <-out)
	}
	return result
}

func (aoc *aoc202323) Part1(input *Input) string {
	return IntResult(aoc.result(aoc.parse(true, input)))
}

func (aoc *aoc202323) Part2(input *Input) string {
	return IntResult(aoc.result(aoc.parse(false, input)))
}
