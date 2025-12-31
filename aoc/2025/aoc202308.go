package main

func init() {
	Register(&aoc202308{
		AOC: AOC{
			Day:           8,
			InputFilename: "../2023/input/08.txt",
			Tests: []Test{
				Test{
					Input: `RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
`,
					Part1: "2",
					Part2: "",
				},
				Test{
					Input: `LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
`,
					Part1: "6",
					Part2: "",
				},
				Test{
					Input: `LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
`,
					Part1: "",
					Part2: "6",
				},
			},
		},
	})
}

type aoc202308 struct {
	AOC
}

func (aoc *aoc202308) parse(input *Input) (string, map[string][2]string) {
	instructions, _ := input.Word()
	graph := map[string][2]string{}
	for {
		w, ok := input.Word()
		if !ok {
			break
		}
		input.Word()
		l, _ := input.Word()
		r, _ := input.Word()
		graph[w] = [2]string{l[1 : len(l)-1], r[:len(r)-1]}
	}
	return instructions, graph
}

func (aoc *aoc202308) Part1(input *Input) string {
	instructions, graph := aoc.parse(input)
	steps := 0
	node := "AAA"
	dirs := instructions
	for node != "ZZZ" {
		if dirs == "" {
			dirs = instructions
		}
		dir := 0
		if dirs[0] == 'R' {
			dir = 1
		}
		dirs = dirs[1:]
		node = graph[node][dir]
		steps++
	}
	return IntResult(steps)
}

func (aoc *aoc202308) Part2(input *Input) string {
	instructions, graph := aoc.parse(input)
	result := 1
	for node, _ := range graph {
		// In my data, the end is reached after an integral number
		// of times through the instructions in every path, and
		// the end then goes to the same node as the start does.
		if node[len(node)-1] == 'A' {
			steps := 0
			n := node
			dirs := instructions
			for n[len(n)-1] != 'Z' {
				if dirs == "" {
					dirs = instructions
				}
				dir := 0
				if dirs[0] == 'R' {
					dir = 1
				}
				dirs = dirs[1:]
				n = graph[n][dir]
				steps++
			}
			result = LCM(result, steps)
		}
	}
	return IntResult(result)
}
