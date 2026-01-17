package main

func init() {
	Register(&aoc202123{
		AOC: AOC{
			Day:           23,
			InputFilename: "../2021/input/23.txt",
			Tests: []Test{
				Test{
					Input: `#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########
`,
					Part1: "12521",
					Part2: "44169",
				},
			},
		},
	})
}

type aoc202123 struct {
	AOC
}

func (aoc *aoc202123) parse(part1 bool, input *Input) [4][]int {
	rooms := [4][]int{}
	i := 0
	for ch := range input.Chars() {
		switch ch {
		case 'A':
			rooms[i%4] = append(rooms[i%4], 1)
			i++
		case 'B':
			rooms[i%4] = append(rooms[i%4], 10)
			i++
		case 'C':
			rooms[i%4] = append(rooms[i%4], 100)
			i++
		case 'D':
			rooms[i%4] = append(rooms[i%4], 1000)
			i++
		}
		if !part1 && i == 4 {
			i = 8
			rooms[0] = append(rooms[0], 1000, 1000)
			rooms[1] = append(rooms[1], 100, 10)
			rooms[2] = append(rooms[2], 10, 1)
			rooms[3] = append(rooms[3], 1, 100)
		}
	}
	for i, c := range [...]int{1, 10, 100, 1000} {
		for rooms[i][len(rooms[i])-1] == c {
			rooms[i] = rooms[i][:len(rooms[i])-1]
		}
	}
	return rooms
}

func (aoc *aoc202123) result(rooms [4][]int) string {
	type path struct {
		hall  [11]int
		rooms [4][]int
		cost  int
	}

	start := []path{path{rooms: rooms}}

	done := func(p path) bool {
		return len(p.rooms[0]) == 0 && len(p.rooms[1]) == 0 && len(p.rooms[2]) == 0 && len(p.rooms[3]) == 0
	}

	priority := func(p path) int {
		priority := -p.cost
		for i, a := range p.hall {
			switch a {
			case 1:
				priority -= a * (1 + max(i-2, 2-i))
			case 10:
				priority -= a * (1 + max(i-4, 4-i))
			case 100:
				priority -= a * (1 + max(i-6, 6-i))
			case 1000:
				priority -= a * (1 + max(i-8, 8-i))
			}
		}
		for i, a := range p.rooms[0] {
			switch a {
			case 1:
				priority -= a * (4 + i)
			case 10:
				priority -= a * (4 + i)
			case 100:
				priority -= a * (6 + i)
			case 1000:
				priority -= a * (8 + i)
			}
		}
		for i, a := range p.rooms[1] {
			switch a {
			case 1:
				priority -= a * (4 + i)
			case 10:
				priority -= a * (4 + i)
			case 100:
				priority -= a * (4 + i)
			case 1000:
				priority -= a * (6 + i)
			}
		}
		for i, a := range p.rooms[2] {
			switch a {
			case 1:
				priority -= a * (6 + i)
			case 10:
				priority -= a * (4 + i)
			case 100:
				priority -= a * (4 + i)
			case 1000:
				priority -= a * (4 + i)
			}
		}
		for i, a := range p.rooms[3] {
			switch a {
			case 1:
				priority -= a * (8 + i)
			case 10:
				priority -= a * (6 + i)
			case 100:
				priority -= a * (4 + i)
			case 1000:
				priority -= a * (4 + i)
			}
		}
		return priority
	}

	state := func(p path) [15]int {
		state := [15]int{}
		copy(state[:], p.hall[:])
		for _, a := range p.rooms[0] {
			state[11] += a
		}
		for _, a := range p.rooms[1] {
			state[12] += a
		}
		for _, a := range p.rooms[2] {
			state[13] += a
		}
		for _, a := range p.rooms[3] {
			state[14] += a
		}
		return state
	}

	moveIn := func(p path, neighbors []path, a int, i int, room int) []path {
		for _, occupant := range p.rooms[room] {
			if occupant != 0 {
				return neighbors
			}
		}
		dest := 2 + 2*room
		if i < dest {
			for j := i + 1; j <= dest; j++ {
				if p.hall[j] != 0 {
					return neighbors
				}
			}
		} else {
			for j := dest; j < i; j++ {
				if p.hall[j] != 0 {
					return neighbors
				}
			}
		}
		neighbor := path{}
		copy(neighbor.hall[:], p.hall[:])
		neighbor.hall[i] = 0
		copy(neighbor.rooms[:], p.rooms[:])
		neighbor.rooms[room] = neighbor.rooms[room][:len(neighbor.rooms[room])-1]
		neighbor.cost = p.cost + a*(max(i-dest, dest-i)+len(p.rooms[room]))
		return append(neighbors, neighbor)
	}

	moveOutTo := func(p path, neighbors []path, room int, j int, a int, i int) []path {
		if i == 2 || i == 4 || i == 6 || i == 8 || i < 0 || i > 10 {
			return neighbors
		}
		neighbor := path{}
		copy(neighbor.hall[:], p.hall[:])
		neighbor.hall[i] = a
		copy(neighbor.rooms[:], p.rooms[:])
		neighbor.rooms[room] = make([]int, len(p.rooms[room]))
		copy(neighbor.rooms[room], p.rooms[room])
		neighbor.rooms[room][j] = 0
		start := 2 + 2*room
		neighbor.cost = p.cost + a*(max(i-start, start-i)+j+1)
		return append(neighbors, neighbor)
	}

	moveOut := func(p path, neighbors []path, room int, j int, a int) []path {
		start := 2 + 2*room
		lblocked, rblocked := false, false
		step := 1
		for {
			if !lblocked {
				if start-step < 0 || p.hall[start-step] != 0 {
					lblocked = true
				} else {
					neighbors = moveOutTo(p, neighbors, room, j, a, start-step)
				}
			}
			if !rblocked {
				if start+step > 10 || p.hall[start+step] != 0 {
					rblocked = true
				} else {
					neighbors = moveOutTo(p, neighbors, room, j, a, start+step)
				}
			}
			step++
			if lblocked && rblocked {
				return neighbors
			}
		}
	}

	neighbors := func(p path) []path {
		neighbors := []path{}
		for i, a := range p.hall {
			switch a {
			case 1:
				neighbors = moveIn(p, neighbors, a, i, 0)
			case 10:
				neighbors = moveIn(p, neighbors, a, i, 1)
			case 100:
				neighbors = moveIn(p, neighbors, a, i, 2)
			case 1000:
				neighbors = moveIn(p, neighbors, a, i, 3)
			}
		}
		for room := range p.rooms {
			for j, a := range p.rooms[room] {
				if a == 0 {
					continue
				}
				neighbors = moveOut(p, neighbors, room, j, a)
				break
			}
		}
		return neighbors
	}

	p, ok := AstarSearch(start, done, priority, state, neighbors)
	if !ok {
		panic("bad input")
	}
	return IntResult(p.cost)
}

func (aoc *aoc202123) Part1(input *Input) string {
	return aoc.result(aoc.parse(true, input))
}

func (aoc *aoc202123) Part2(input *Input) string {
	return aoc.result(aoc.parse(false, input))
}
