package main

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
	dest  XYDir
	score int
	tiles []XY
}

func (aoc *aoc202416) parse(input *Input) (XYDir, XY, map[XYDir]*aoc202416_edge) {
	w, h, grid := input.Grid()
	start := XYDir{}
	goal := XY{}

	queue := NewQueue[XYDir]()
	for x := range w {
		for y := range h {
			xy := XY{x, y}
			if grid[xy] == '#' {
				continue
			} else if grid[xy] == 'E' {
				goal = XY{x, y}
				continue
			} else if grid[xy] == 'S' {
				start = ToXYDir(xy, DirR)
			}
			exits := 0
			for _, dir := range [...]int{DirR, DirD, DirL, DirU} {
				if grid[AdvanceXY(xy, dir, 1)] != '#' {
					exits++
				}
			}
			if grid[xy] != 'S' && exits < 3 {
				continue
			}
			for _, dir := range [...]int{DirR, DirD, DirL, DirU} {
				xydir := ToXYDir(xy, dir)
				if grid[ToXY(AdvanceXYDir(xydir, 1))] != '#' {
					queue.Enqueue(xydir)
				}
			}
		}
	}

	graph := map[XYDir]*aoc202416_edge{}
dequeueLoop:
	for !queue.Empty() {
		xydir := queue.Dequeue()
		edge := &aoc202416_edge{
			dest:  AdvanceXYDir(xydir, 1),
			score: 1,
			tiles: nil,
		}
		for {
			switch grid[ToXY(edge.dest)] {
			case 'S', 'E':
				graph[xydir] = edge
				continue dequeueLoop
			}
			edge.tiles = append(edge.tiles, ToXY(edge.dest))

			turn := 0
			exits := 0
			nextDest := edge.dest
			if grid[ToXY(AdvanceXYDir(edge.dest, 1))] != '#' {
				exits++
				nextDest = AdvanceXYDir(edge.dest, 1)
			}
			if grid[ToXY(AdvanceXYDir(XYDirTurnL(edge.dest), 1))] != '#' {
				exits++
				nextDest = AdvanceXYDir(XYDirTurnL(edge.dest), 1)
				turn = 1000
			}
			if grid[ToXY(AdvanceXYDir(XYDirTurnR(edge.dest), 1))] != '#' {
				exits++
				nextDest = AdvanceXYDir(XYDirTurnR(edge.dest), 1)
				turn = 1000
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

	return start, goal, graph
}

func (aoc *aoc202416) result(part1 bool, input *Input) string {
	start, goal, graph := aoc.parse(input)

	type path struct {
		xydir    XYDir
		score    int
		lastEdge *aoc202416_edge
		lastPath *path
	}

	done := func(p path) bool {
		return ToXY(p.xydir) == goal
	}

	priority := func(p path) int {
		priority := -p.score
		dx := goal[0] - p.xydir[0]
		dy := goal[1] - p.xydir[1]
		priority -= max(dx, -dx) + max(dy, -dy)
		if dx != 0 && dy != 0 {
			priority -= 1000
		}
		return priority
	}

	state := func(p path) XYDir {
		return p.xydir
	}

	neighbors := func(p path) []path {
		edge := graph[p.xydir]
		if edge == nil {
			return []path{
				path{
					xydir:    XYDirTurnL(p.xydir),
					score:    p.score + 1000,
					lastEdge: nil,
					lastPath: &p,
				},
				path{
					xydir:    XYDirTurnR(p.xydir),
					score:    p.score + 1000,
					lastEdge: nil,
					lastPath: &p,
				},
			}
		} else {
			return []path{
				path{
					xydir:    edge.dest,
					score:    p.score + edge.score,
					lastEdge: edge,
					lastPath: &p,
				},
				path{
					xydir:    XYDirTurnL(p.xydir),
					score:    p.score + 1000,
					lastEdge: nil,
					lastPath: &p,
				},
				path{
					xydir:    XYDirTurnR(p.xydir),
					score:    p.score + 1000,
					lastEdge: nil,
					lastPath: &p,
				},
			}
		}
	}

	if part1 {
		p, ok := AstarSearch([]path{path{xydir: start}}, done, priority, state, neighbors)
		if !ok {
			panic("bad input")
		}
		return IntResult(p.score)
	} else {
		ps := AstarSearchAll([]path{path{xydir: start}}, done, priority, state, neighbors)
		tiles := map[XY]bool{}
		for _, pp := range ps {
			for p := &pp; p != nil; p = p.lastPath {
				if p.lastEdge != nil {
					for _, tile := range p.lastEdge.tiles {
						tiles[tile] = true
					}
				}
			}
		}
		return IntResult(len(tiles) + 2)
	}
}

func (aoc *aoc202416) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202416) Part2(input *Input) string {
	return aoc.result(false, input)
}
