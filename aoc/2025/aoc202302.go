package main

func init() {
	Register(&aoc202302{
		AOC: AOC{
			Day:           2,
			InputFilename: "../2023/input/02.txt",
			Tests: []Test{
				Test{
					Input: `Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
`,
					Part1: "8",
					Part2: "2286",
				},
			},
		},
	})
}

type aoc202302 struct {
	AOC
}

func (aoc *aoc202302) next(input *Input) (int, [][3]int, bool) {
	line, ok := input.Line()
	if !ok {
		return 0, nil, false
	}
	input = InputString(line)
	gameID, _ := input.Int()
	input.Char()
	counts := [][3]int{}
grabLoop:
	for {
		count := [3]int{}
		for {
			n, _ := input.Int()
			color, ok := input.Word()
			if !ok {
				break grabLoop
			}
			if color[0] == 'r' {
				count[0] = n
			} else if color[0] == 'g' {
				count[1] = n
			} else if color[0] == 'b' {
				count[2] = n
			}
			if color[len(color)-1] != ',' {
				break
			}
		}
		counts = append(counts, count)
	}
	return gameID, counts, true
}

func (aoc *aoc202302) Part1(input *Input) string {
	result := 0
	for gameID, counts, ok := aoc.next(input); ok; gameID, counts, ok = aoc.next(input) {
		possible := true
		for _, count := range counts {
			if count[0] > 12 || count[1] > 13 || count[2] > 14 {
				possible = false
				break
			}
		}
		if possible {
			result += gameID
		}
	}
	return IntResult(result)
}

func (aoc *aoc202302) Part2(input *Input) string {
	result := 0
	for _, counts, ok := aoc.next(input); ok; _, counts, ok = aoc.next(input) {
		r, g, b := 0, 0, 0
		for _, count := range counts {
			r, g, b = max(r, count[0]), max(g, count[1]), max(b, count[2])
		}
		result += r * g * b
	}
	return IntResult(result)
}
