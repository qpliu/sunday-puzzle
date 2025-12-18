package main

import (
	"container/list"
	"fmt"
)

func init() {
	Register(&aoc202416{
		AOC: AOC{
			Day:           16,
			InputFilename: "../2024/input/16.txt",
			Tests: []Test{
				Test{
					Input: `###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
`,
					Part1: "7036",
					Part2: "45",
				},
				Test{
					Input: `#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
`,
					Part1: "11048",
					Part2: "64",
				},
			},
		},
	})
}

type aoc202416 struct {
	AOC
}

type aoc202416_edge struct {
	dest  [3]int
	score int
	tiles [][2]int
}

type aoc202416_path struct {
	goal     [2]int
	graph    map[[3]int]*aoc202416_edge
	xydir    [3]int
	score    int
	lastEdge *aoc202416_edge
	lastPath *aoc202416_path
}

func (p aoc202416_path) String() string {
	dir := '?'
	switch p.xydir[2] {
	case 0:
		dir = '^'
	case 1:
		dir = '>'
	case 2:
		dir = 'v'
	case 3:
		dir = '<'
	}
	return fmt.Sprintf("(%d:%d,%d%c)", p.score, p.xydir[0], p.xydir[1], dir)
}

func (p aoc202416_path) Done() bool {
	return p.xydir[0] == p.goal[0] && p.xydir[1] == p.goal[1]
}

func (p aoc202416_path) Priority() int {
	priority := -p.score
	dx := max(p.goal[0]-p.xydir[0], p.xydir[0]-p.goal[0])
	dy := max(p.goal[1]-p.xydir[1], p.xydir[1]-p.goal[1])
	priority -= dx + dy
	if dx > 0 && dy > 0 {
		priority -= 1000
	}
	return priority
}

func (p aoc202416_path) State() [3]int {
	return p.xydir
}

func (p aoc202416_path) Neighbors() []AstarPath[[3]int] {
	edge := p.graph[p.xydir]
	if edge == nil {
		return []AstarPath[[3]int]{
			aoc202416_path{
				goal:     p.goal,
				graph:    p.graph,
				xydir:    [3]int{p.xydir[0], p.xydir[1], (p.xydir[2] + 1) % 4},
				score:    p.score + 1000,
				lastEdge: nil,
				lastPath: &p,
			},
			aoc202416_path{
				goal:     p.goal,
				graph:    p.graph,
				xydir:    [3]int{p.xydir[0], p.xydir[1], (p.xydir[2] + 3) % 4},
				score:    p.score + 1000,
				lastEdge: nil,
				lastPath: &p,
			},
		}
	} else {
		return []AstarPath[[3]int]{
			aoc202416_path{
				goal:     p.goal,
				graph:    p.graph,
				xydir:    edge.dest,
				score:    p.score + edge.score,
				lastEdge: edge,
				lastPath: &p,
			},
			aoc202416_path{
				goal:     p.goal,
				graph:    p.graph,
				xydir:    [3]int{p.xydir[0], p.xydir[1], (p.xydir[2] + 1) % 4},
				score:    p.score + 1000,
				lastEdge: nil,
				lastPath: &p,
			},
			aoc202416_path{
				goal:     p.goal,
				graph:    p.graph,
				xydir:    [3]int{p.xydir[0], p.xydir[1], (p.xydir[2] + 3) % 4},
				score:    p.score + 1000,
				lastEdge: nil,
				lastPath: &p,
			},
		}
	}
}

func (aoc *aoc202416) parse(input *Input) []AstarPath[[3]int] {
	w, h, grid := input.Grid()
	start := aoc202416_path{}
	queue := list.New()
	for x := range w {
		for y := range h {
			xy := [2]int{x, y}
			if grid[xy] == '#' {
				continue
			} else if grid[xy] == 'E' {
				start.goal = [2]int{x, y}
				continue
			} else if grid[xy] == 'S' {
				start.xydir = [3]int{x, y, 1}
			}
			exits := 0
			if grid[[2]int{x + 1, y}] != '#' {
				exits++
			}
			if grid[[2]int{x - 1, y}] != '#' {
				exits++
			}
			if grid[[2]int{x, y + 1}] != '#' {
				exits++
			}
			if grid[[2]int{x, y - 1}] != '#' {
				exits++
			}
			if grid[xy] != 'S' && exits < 3 {
				continue
			}
			if grid[[2]int{x + 1, y}] != '#' {
				queue.PushFront([3]int{x, y, 1})
			}
			if grid[[2]int{x - 1, y}] != '#' {
				queue.PushFront([3]int{x, y, 3})
			}
			if grid[[2]int{x, y + 1}] != '#' {
				queue.PushFront([3]int{x, y, 2})
			}
			if grid[[2]int{x, y - 1}] != '#' {
				queue.PushFront([3]int{x, y, 0})
			}
		}
	}
	graph := map[[3]int]*aoc202416_edge{}
dequeueLoop:
	for queue.Len() > 0 {
		e := queue.Front()
		queue.Remove(e)
		xydir := e.Value.([3]int)
		edge := &aoc202416_edge{
			dest:  xydir,
			score: 1,
			tiles: nil,
		}
		switch edge.dest[2] {
		case 0:
			edge.dest[1]--
		case 1:
			edge.dest[0]++
		case 2:
			edge.dest[1]++
		case 3:
			edge.dest[0]--
		}
		for {
			switch grid[[2]int{edge.dest[0], edge.dest[1]}] {
			case 'S', 'E':
				graph[xydir] = edge
				continue dequeueLoop
			}
			edge.tiles = append(edge.tiles, [2]int{edge.dest[0], edge.dest[1]})

			turn := 0
			exits := 0
			nextDest := edge.dest
			for _, dest := range [4][3]int{
				[3]int{edge.dest[0], edge.dest[1] - 1, 0},
				[3]int{edge.dest[0] + 1, edge.dest[1], 1},
				[3]int{edge.dest[0], edge.dest[1] + 1, 2},
				[3]int{edge.dest[0] - 1, edge.dest[1], 3},
			} {
				if edge.dest[2] == (dest[2]+2)%4 {
					// u-turn
					continue
				}
				if grid[[2]int{dest[0], dest[1]}] == '#' {
					continue
				}
				exits++
				nextDest = dest
				if nextDest[2] != edge.dest[2] {
					turn = 1000
				}
			}
			switch exits {
			case 0:
				continue dequeueLoop // dead end
			case 1:
				edge.score += 1 + turn
				edge.dest = nextDest
			default:
				graph[xydir] = edge
				continue dequeueLoop
			}
		}
	}
	start.graph = graph
	return []AstarPath[[3]int]{start}
}

func (aoc *aoc202416) Part1(input *Input) string {
	path := AstarSearch(aoc.parse(input))
	if path != nil {
		return IntResult(path.(aoc202416_path).score)
	}
	return IntResult(0)
}

func (aoc *aoc202416) Part2(input *Input) string {
	paths := AstarSearchAll(aoc.parse(input))
	tiles := map[[2]int]bool{}
	for _, path := range paths {
		pp := path.(aoc202416_path)
		for p := &pp; p != nil; p = p.lastPath {
			if p.lastEdge == nil {
				continue
			}
			for _, tile := range p.lastEdge.tiles {
				tiles[tile] = true
			}
		}
	}
	return IntResult(len(tiles) + 2)
}
