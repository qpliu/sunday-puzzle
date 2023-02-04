package main

const (
	north = 1
	south = 2
	west  = 3
	east  = 4

	wall  = 0
	moved = 1
	goal  = 2

	nKnown   = 1
	sKnown   = 2
	wKnown   = 4
	eKnown   = 8

	nWall    = 16
	sWall    = 32
	wWall    = 64
	eWall    = 128
)

// depth-first
func makeMap(ic *intcode) ([2]int, map[[2]int]byte) {
	goalxy := [2]int{0, 0}
	xy := [2]int{0, 0}
	command := int64(1)
	backtrack := []int64{}
	m := map[[2]int]byte{}
	xymin := [2]int{0, 0}
	xymax := [2]int{0, 0}
	backtracking := false
	ic.init()
	ic.interpIO(func() (bool, int64) {
		return false, command
	}, func(out int64) bool {
		switch out {
		case wall:
			m[xy] |= (1 << byte(command-1)) | (8 << byte(command))
		case goal, moved:
			m[xy] |= (1 << byte(command-1))
			switch command {
			case north:
				xy[1]--
				if xy[1] < xymin[1] {
					xymin[1] = xy[1]
				}
			case south:
				xy[1]++
				if xy[1] > xymax[1] {
					xymax[1] = xy[1]
				}
			case west:
				xy[0]--
				if xy[0] < xymin[0] {
					xymin[0] = xy[0]
				}
			case east:
				xy[0]++
				if xy[0] > xymax[0] {
					xymax[0] = xy[0]
				}
			}
			m[xy] |= (1 << byte((command-1)^1))
			if !backtracking {
				backtrack = append(backtrack, (command-1)^1+1)
			}
			if out == goal {
				goalxy = xy
			}
		}
		mxy := m[xy]
		if mxy&nKnown == 0 {
			command = north
			backtracking = false
		} else if mxy&sKnown == 0 {
			command = south
			backtracking = false
		} else if mxy&wKnown == 0 {
			command = west
			backtracking = false
		} else if mxy&eKnown == 0 {
			command = east
			backtracking = false
		} else if len(backtrack) > 0 {
			command = backtrack[len(backtrack)-1]
			backtrack = backtrack[:len(backtrack)-1]
			backtracking = true
		} else {
			return true
		}
		return false
	})
	println("goal:",goalxy[0]," ", goalxy[1]," size:", len(m)," xmin:", xymin[0]," ymin:", xymin[1])
	for y := xymin[1]; y <= xymax[1]; y++ {
		for x := xymin[0]; x <= xymax[0]; x++ {
			if x == 0 && y == 0 {
				print("0")
				continue
			} else if x == goalxy[0] && y == goalxy[1] {
				print("*")
				continue
			}
			switch m[[2]int{x,y}] {
			case 0x0f:
				print("┼")
			case 0x1f:
				print("┬")
			case 0x2f:
				print("┴")
			case 0x4f:
				print("├")
			case 0x8f:
				print("┤")
			case 0x3f:
				print("─")
			case 0x5f:
				print("┌")
			case 0x9f:
				print("┐")
			case 0x6f:
				print("└")
			case 0xaf:
				print("┘")
			case 0xcf:
				print("│")
			case 0xef, 0xdf, 0xbf, 0x7f:
				print(".")
			case 0xff:
				print("X")
			default:
				print("?")
			}
		}
		println()
	}
	return goalxy, m
}

// breadth-first
func search(goalxy [2]int, m map[[2]int]byte) int {
	moves := 0
	current := [][2]int{[2]int{0, 0}}
	next := [][2]int{}
	for {
		for _, xy := range current {
			if xy == goalxy {
				return moves
			}
			mxy := m[xy]
			if mxy&1 == 0 {
				continue
			}
			m[xy] &= 254
			if mxy&nWall == 0 {
				next = append(next, [2]int{xy[0], xy[1] - 1})
			}
			if mxy&sWall == 0 {
				next = append(next, [2]int{xy[0], xy[1] + 1})
			}
			if mxy&wWall == 0 {
				next = append(next, [2]int{xy[0] - 1, xy[1]})
			}
			if mxy&eWall == 0 {
				next = append(next, [2]int{xy[0] + 1, xy[1]})
			}
		}
		moves++
		current, next = next, current[:0]
	}
}

func fill(goalxy [2]int, m map[[2]int]byte) int {
	moves := 0
	current := [][2]int{goalxy}
	next := [][2]int{}
	for {
		for _, xy := range current {
			mxy := m[xy]
			if mxy&2 == 0 {
				continue
			}
			m[xy] &= 253
			if mxy&nWall == 0 && m[[2]int{xy[0], xy[1] - 1}]&2 != 0{
				next = append(next, [2]int{xy[0], xy[1] - 1})
			}
			if mxy&sWall == 0 && m[[2]int{xy[0], xy[1] + 1}]&2 != 0 {
				next = append(next, [2]int{xy[0], xy[1] + 1})
			}
			if mxy&wWall == 0 && m[[2]int{xy[0] - 1, xy[1]}]&2 != 0 {
				next = append(next, [2]int{xy[0] - 1, xy[1]})
			}
			if mxy&eWall == 0 && m[[2]int{xy[0] + 1, xy[1]}]&2 != 0 {
				next = append(next, [2]int{xy[0] + 1, xy[1]})
			}
		}
		if len(next) == 0 {
			return moves
		}
		moves++
		current, next = next, current[:0]
	}
}

func main() {
	goalxy, m := makeMap(parseFile("input/15.txt"))

	// Part 1
	println(search(goalxy, m))

	// Part 2
	println(fill(goalxy, m))
}
