package main

func init() {
	Register(&aoc202501{
		AOC: AOC{
			Day: 1,
			Tests: []Test{
				Test{
					Input: `L68
L30
R48
L5
R60
L55
L1
L99
R14
L82`,
					Part1: "3",
					Part2: "6",
				},
			},
		},
	})
}

type aoc202501 struct {
	AOC
}

func (aoc *aoc202501) nextTurn(input *Input) (int, bool) {
	line, ok := input.Line()
	if !ok {
		return 0, false
	}
	turn, _ := InputString(line).Int()
	if line[0] == 'L' {
		turn = -turn
	}
	return turn, true
}

func (aoc *aoc202501) Part1(input *Input) string {
	dial := 50
	clicks := 0
	for turn, ok := aoc.nextTurn(input); ok; turn, ok = aoc.nextTurn(input) {
		dial = (dial + turn) % 100
		if dial == 0 {
			clicks++
		}
	}
	return IntResult(clicks)
}

func (aoc aoc202501) Part2(input *Input) string {
	dial := 50
	clicks := 0
	for turn, ok := aoc.nextTurn(input); ok; turn, ok = aoc.nextTurn(input) {
		if turn < 0 {
			clicks += -turn / 100
			turn += 100 * (-turn / 100)
			if dial == 0 {
				dial = 100 + turn
			} else {
				dial += turn
				if dial == 0 {
					clicks++
				} else if dial < 0 {
					clicks++
					dial += 100
				}
			}
		} else {
			if turn >= 100 {
				clicks += turn / 100
				turn %= 100
			}
			dial += turn
			if dial >= 100 {
				dial %= 100
				clicks++
			}
		}
	}
	return IntResult(clicks)
}
