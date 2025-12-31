package main

func init() {
	Register(&aoc202303{
		AOC: AOC{
			Day:           3,
			InputFilename: "../2023/input/03.txt",
			Tests: []Test{
				Test{
					Input: `467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
`,
					Part1: "4361",
					Part2: "467835",
				},
			},
		},
	})
}

type aoc202303 struct {
	AOC
}

func (aoc *aoc202303) digit(b byte) (int, bool) {
	switch b {
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		return int(b - '0'), true
	}
	return 0, false
}

func (aoc *aoc202303) symbol(b byte) bool {
	switch b {
	case 0, '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.':
		return false
	}
	return true
}

func (aoc *aoc202303) Part1(input *Input) string {
	_, _, grid := input.Grid()

	result := 0
	for xy, b := range grid {
		n, isDigit := aoc.digit(b)
		if !isDigit {
			continue
		}
		x, y := xy[0], xy[1]
		if _, isDigit := aoc.digit(grid[[2]int{x - 1, y}]); isDigit {
			continue
		}
		hasSymbol := aoc.symbol(grid[[2]int{x - 1, y}])
		hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x - 1, y - 1}])
		hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x - 1, y + 1}])
		hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x, y - 1}])
		hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x, y + 1}])
		for {
			x++
			hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x, y - 1}])
			hasSymbol = hasSymbol || aoc.symbol(grid[[2]int{x, y + 1}])
			b = grid[[2]int{x, y}]
			hasSymbol = hasSymbol || aoc.symbol(b)
			d, isDigit := aoc.digit(b)
			if !isDigit {
				break
			}
			n = n*10 + d
		}
		if hasSymbol {
			result += n
		}
	}
	return IntResult(result)
}

func (aoc *aoc202303) Part2(input *Input) string {
	_, _, grid := input.Grid()

	gears := map[[2]int][]int{}
	for xy, b := range grid {
		n, isDigit := aoc.digit(b)
		if !isDigit {
			continue
		}
		x, y := xy[0], xy[1]
		if _, isDigit := aoc.digit(grid[[2]int{x - 1, y}]); isDigit {
			continue
		}
		gearlocs := map[[2]int]bool{}
		if grid[[2]int{x - 1, y}] == '*' {
			gearlocs[[2]int{x - 1, y}] = true
		}
		if grid[[2]int{x - 1, y - 1}] == '*' {
			gearlocs[[2]int{x - 1, y - 1}] = true
		}
		if grid[[2]int{x - 1, y + 1}] == '*' {
			gearlocs[[2]int{x - 1, y + 1}] = true
		}
		if grid[[2]int{x, y - 1}] == '*' {
			gearlocs[[2]int{x, y - 1}] = true
		}
		if grid[[2]int{x, y + 1}] == '*' {
			gearlocs[[2]int{x, y + 1}] = true
		}
		for {
			x++
			if grid[[2]int{x, y - 1}] == '*' {
				gearlocs[[2]int{x, y - 1}] = true
			}
			if grid[[2]int{x, y + 1}] == '*' {
				gearlocs[[2]int{x, y + 1}] = true
			}
			b = grid[[2]int{x, y}]
			if b == '*' {
				gearlocs[[2]int{x, y}] = true
			}
			d, isDigit := aoc.digit(b)
			if !isDigit {
				break
			}
			n = n*10 + d
		}
		for loc := range gearlocs {
			gears[loc] = append(gears[loc], n)
		}
	}

	result := 0
	for _, gear := range gears {
		if len(gear) == 2 {
			result += gear[0] * gear[1]
		}
	}
	return IntResult(result)
}
