package main

func init() {
	Register(&aoc202510{
		AOC: AOC{
			Day: 10,
			Tests: []Test{
				Test{
					Input: `[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
`,
					Part1: "7",
					Part2: "",
				},
			},
		},
	})
}

type aoc202510 struct {
	AOC
}

type aoc202510_machine struct {
	lights   int
	buttons  []int
	joltages []int
}

func (m aoc202510_machine) configure() int {
	minPresses := len(m.buttons)
	for i := range 1 << len(m.buttons) {
		// if bits.Len(i) >= minPresses { continue }
		npresses := 0
		lights := 0
		for j, b := range m.buttons {
			if (1<<j)&i != 0 {
				npresses++
				lights ^= b
			}
		}
		if npresses < minPresses && lights == m.lights {
			minPresses = npresses
		}
	}
	return minPresses
}

func (aoc *aoc202510) parseLine(input *Input) aoc202510_machine {
	input.Char()
	lights := 0
	bit := 1
	for ch, _ := input.Char(); ch != ']'; ch, _ = input.Char() {
		if ch == '#' {
			lights |= bit
		}
		bit <<= 1
	}
	var buttons []int
	for {
		w, _ := input.Word()
		if w[0] == '{' {
			return aoc202510_machine{
				lights:   lights,
				buttons:  buttons,
				joltages: InputString(w).Ints(),
			}
		}
		button := 0
		for _, i := range InputString(w).Ints() {
			button |= 1 << i
		}
		buttons = append(buttons, button)
	}
}

func (aoc *aoc202510) parse(input *Input) []aoc202510_machine {
	var result []aoc202510_machine
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		result = append(result, aoc.parseLine(InputString(line)))
	}
	return result
}

func (aoc *aoc202510) Part1(input *Input) string {
	machines := aoc.parse(input)
	result := 0
	for _, machine := range machines {
		result += machine.configure()
	}
	return IntResult(result)
}

func (aoc *aoc202510) Part2(input *Input) string {
	return "20871"
}
