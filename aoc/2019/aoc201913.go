package main

func part1() {
	screen := map[[2]int64]int64{}
	ic := parseFile("input/13.txt")
	x, y, state := int64(0), int64(0), 0
	ic.init()
	ic.interpIO(func() (bool, int64) {
		return false, 0
	}, func(out int64) bool {
		switch state {
		case 0:
			x, state = out, 1
		case 1:
			y, state = out, 2
		case 2:
			screen[[2]int64{x, y}], state = out, 0
		}
		return false
	})
	count := 0
	for _, tile := range screen {
		if tile == 2 {
			count++
		}
	}
	println(count)
}

func part2() {
	screen := map[[2]int64]int64{}
	ic := parseFile("input/13.txt")
	x, y, score, state := int64(0), int64(0), int64(0), 0
	ballx, paddlex := int64(0), int64(0)

	ic.code[0] = 2
	ic.init()
	ic.interpIO(func() (bool, int64) {
		for {
			if paddlex > ballx {
				return false, -1
			} else if paddlex == ballx {
				return false, 0
			} else {
				return false, 1
			}
		}
	}, func(out int64) bool {
		switch state {
		case 0:
			x, state = out, 1
		case 1:
			y, state = out, 2
		case 2:
			if x == -1 && y == 0 {
				score = out
			} else {
				screen[[2]int64{x, y}] = out
				switch out {
				case 3:
					paddlex = x
				case 4:
					ballx = x
				}
			}
			state = 0
		}
		return false
	})
	println(score)
}

func main() {
	part1()
	part2()
}
