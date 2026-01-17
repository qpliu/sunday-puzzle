package main

func init() {
	Register(&aoc202121{
		AOC: AOC{
			Day:           21,
			InputFilename: "../2021/input/21.txt",
			Tests: []Test{
				Test{
					Input: `Player 1 starting position: 4
Player 2 starting position: 8
`,
					Part1: "739785",
					Part2: "444356092776315",
				},
			},
		},
	})
}

type aoc202121 struct {
	AOC
}

func (aoc *aoc202121) Part1(input *Input) string {
	start := input.Ints()
	p1, p2 := start[1]-1, start[3]-1
	s1, s2 := 0, 0
	die := 0
	turns := 1
	for {
		p1 = (p1 + 3*die + 6) % 10
		s1 += p1 + 1
		if s1 >= 1000 {
			break
		}
		die = (die + 3) % 100
		turns++
		p2 = (p2 + 3*die + 6) % 10
		s2 += p2 + 1
		if s2 >= 1000 {
			break
		}
		die = (die + 3) % 100
		turns++
	}
	return IntResult(3 * turns * min(s1, s2))
}

func (aoc *aoc202121) Part2(input *Input) string {
	memo := map[[4]int][2]int{}
	var wins func(int, int, int, int) [2]int
	wins = func(p1, s1, p2, s2 int) [2]int {
		k := [4]int{p1, s1, p2, s2}
		results, ok := memo[k]
		if ok {
			return results
		}
		for _, rolls := range [...][2]int{
			[2]int{3, 1}, // 3 - 1: 111
			[2]int{4, 3}, // 4 - 3: 112 121 211
			[2]int{5, 6}, // 5 - 6: 113 131 311 122 212 211
			[2]int{6, 7}, // 6 - 7: 123 132 213 231 312 321 222
			[2]int{7, 6}, // 7 - 6: 133 313 331 223 232 322
			[2]int{8, 3}, // 8 - 3: 233 323 332
			[2]int{9, 1}, // 9 - 1: 333
		} {
			newP1 := (p1 + rolls[0]) % 10
			newS1 := s1 + newP1 + 1
			if newS1 >= 21 {
				results[0] += rolls[1]
			} else {
				r := wins(p2, s2, newP1, newS1)
				results[0] += rolls[1] * r[1]
				results[1] += rolls[1] * r[0]
			}
		}
		memo[k] = results
		return results
	}

	start := input.Ints()
	results := wins(start[1]-1, 0, start[3]-1, 0)
	return IntResult(max(results[0], results[1]))
}
