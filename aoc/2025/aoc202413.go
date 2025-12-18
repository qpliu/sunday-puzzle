package main

func init() {
	Register(&aoc202413{
		AOC: AOC{
			Day:           13,
			InputFilename: "../2024/input/13.txt",
			Tests: []Test{
				Test{
					Input: `Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
`,
					Part1: "480",
					Part2: "",
				},
			},
		},
	})
}

type aoc202413 struct {
	AOC
}

func (aoc *aoc202413) next(conversion int, input *Input) (int, bool) {
	ax, ok := input.Int()
	if !ok {
		return 0, false
	}
	ay, _ := input.Int()
	bx, _ := input.Int()
	by, _ := input.Int()
	px, _ := input.Int()
	py, _ := input.Int()
	px += conversion
	py += conversion

	// na == -(by*px - bx*py)/(ay*bx - ax*by)
	// nb == (ay*px - ax*py)/(ay*bx - ax*by)
	denominator := ay*bx - ax*by
	if denominator == 0 {
		return 0, true
	}
	numeratora := -by*px + bx*py
	if numeratora%denominator != 0 {
		return 0, true
	}
	na := numeratora / denominator
	if na < 0 {
		return 0, true
	}
	numeratorb := ay*px - ax*py
	if numeratorb%denominator != 0 {
		return 0, true
	}
	nb := numeratorb / denominator
	if nb < 0 {
		return 0, true
	}
	return 3*na + nb, true
}

func (aoc *aoc202413) run(conversion int, input *Input) int {
	result := 0
	for tokens, ok := aoc.next(conversion, input); ok; tokens, ok = aoc.next(conversion, input) {
		result += tokens
	}
	return result
}

func (aoc *aoc202413) Part1(input *Input) string {
	return IntResult(aoc.run(0, input))
}

func (aoc *aoc202413) Part2(input *Input) string {
	return IntResult(aoc.run(10000000000000, input))
}
