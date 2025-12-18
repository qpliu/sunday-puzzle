package main

func init() {
	Register(&aoc202425{
		AOC: AOC{
			Day:           25,
			InputFilename: "../2024/input/25.txt",
			Tests: []Test{
				Test{
					Input: `#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
`,
					Part1: "3",
					Part2: "",
				},
			},
		},
	})
}

type aoc202425 struct {
	AOC
}

func (aoc *aoc202425) parse(input *Input) ([][5]int, [][5]int) {
	locks := [][5]int{}
	keys := [][5]int{}
	for {
		line, ok := input.Line()
		if !ok {
			return locks, keys
		}
		if line == "" {
			continue
		}
		if line == "#####" {
			lock := [5]int{}
			for {
				line, ok := input.Line()
				if !ok || line == "" {
					break
				}
				for i := range 5 {
					if line[i] == '#' {
						lock[i]++
					}
				}
			}
			locks = append(locks, lock)
		} else if line == "....." {
			key := [5]int{}
			for {
				line, ok := input.Line()
				if !ok || line == "" {
					break
				}
				for i := range 5 {
					if line[i] == '.' {
						key[i]++
					}
				}
			}
			keys = append(keys, key)
		}
	}
}

func (aoc *aoc202425) Part1(input *Input) string {
	locks, keys := aoc.parse(input)
	result := 0
	for _, lock := range locks {
		for _, key := range keys {
			if lock[0] <= key[0] && lock[1] <= key[1] && lock[2] <= key[2] && lock[3] <= key[3] && lock[4] <= key[4] {
				result++
			}
		}
	}
	return IntResult(result)
}

func (aoc *aoc202425) Part2(input *Input) string {
	return ""
}
