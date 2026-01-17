package main

func init() {
	Register(&aoc202102{
		AOC: AOC{
			Day:           2,
			InputFilename: "../2021/input/02.txt",
			Tests: []Test{
				Test{
					Input: `forward 5
down 5
forward 8
up 3
down 8
forward 2
`,
					Part1: "150",
					Part2: "900",
				},
			},
		},
	})
}

type aoc202102 struct {
	AOC
}

func (aoc *aoc202102) Part1(input *Input) string {
	x, y := 0, 0
	for {
		dir, ok := input.Word()
		if !ok {
			break
		}
		n, _ := input.Int()
		switch dir {
		case "forward":
			x += n
		case "up":
			y -= n
		case "down":
			y += n
		}
	}
	return IntResult(x * y)
}

func (aoc *aoc202102) Part2(input *Input) string {
	x, y, aim := 0, 0, 0
	for {
		dir, ok := input.Word()
		if !ok {
			break
		}
		n, _ := input.Int()
		switch dir {
		case "forward":
			x += n
			y += n * aim
		case "up":
			aim -= n
		case "down":
			aim += n
		}
	}
	return IntResult(x * y)
}
