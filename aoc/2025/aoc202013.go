package main

func init() {
	Register(&aoc202013{
		AOC: AOC{
			Day:           13,
			InputFilename: "../2020/input/13.txt",
			Tests: []Test{
				Test{
					Input: `939
7,13,x,x,59,x,31,19
`,
					Part1: "295",
					Part2: "1068781",
				},
				Test{
					Input: `939
17,x,13,19`,
					Part1: "",
					Part2: "3417",
				},
				Test{
					Input: `939
67,7,59,61`,
					Part1: "",
					Part2: "754018",
				},
				Test{
					Input: `939
67,x,7,59,61`,
					Part1: "",
					Part2: "779210",
				},
				Test{
					Input: `939
67,7,x,59,61`,
					Part1: "",
					Part2: "1261476",
				},
				Test{
					Input: `939
1789,37,47,1889`,
					Part1: "",
					Part2: "1202161486",
				},
				Test{
					Input: `939
`,
					Part1: "",
					Part2: "",
				},
			},
		},
	})
}

type aoc202013 struct {
	AOC
}

func (aoc *aoc202013) Part1(input *Input) string {
	t0, _ := input.Int()
	firstT := 0
	firstBus := 0
	for bus := range input.IntSeq() {
		t := ((t0 + bus - 1) / bus) * bus
		if firstT == 0 || firstT > t {
			firstT = t
			firstBus = bus
		}
	}
	return IntResult(firstBus * (firstT - t0))
}

func (aoc *aoc202013) Part2(input *Input) string {
	input.Line()
	t0 := 0
	recur := [2]int{0, 1}
	for {
		bus, ok := input.Int()
		if !ok {
			break
		}
		recur = Convergences(recur, [2]int{bus + (-t0)%bus, bus})
		t0++
		for input.Skip(",x") {
			t0++
		}
	}
	return IntResult(recur[0])
}
