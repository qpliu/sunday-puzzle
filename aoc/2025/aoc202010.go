package main

import (
	"slices"
)

func init() {
	Register(&aoc202010{
		AOC: AOC{
			Day:           10,
			InputFilename: "../2020/input/10.txt",
			Tests: []Test{
				Test{
					Input: `16
10
15
5
1
11
7
19
6
12
4
`,
					Part1: "35",
					Part2: "8",
				},
				Test{
					Input: `28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
`,
					Part1: "220",
					Part2: "19208",
				},
			},
		},
	})
}

type aoc202010 struct {
	AOC
}

func (aoc *aoc202010) Part1(input *Input) string {
	adapters := input.Ints()
	slices.Sort(adapters)
	one := 0
	three := 1
	jolts := 0
	for _, adapter := range adapters {
		switch adapter - jolts {
		case 1:
			one++
		case 3:
			three++
		default:
			panic("bad input")
		}
		jolts = adapter
	}
	return IntResult(one * three)
}

func (aoc *aoc202010) Part2(input *Input) string {
	set := map[int]bool{}
	jmax := 0
	for adapter := range input.IntSeq() {
		set[adapter] = true
		jmax = max(jmax, adapter)
	}

	memo := map[int]int{}
	var ways func(int) int
	ways = func(jolts int) int {
		if w, ok := memo[jolts]; ok {
			return w
		}
		if jolts == jmax {
			memo[jolts] = 1
			return 1
		}
		w := 0
		for delta := 1; delta <= 3; delta++ {
			if set[jolts+delta] {
				w += ways(jolts + delta)
			}
		}
		memo[jolts] = w
		return w
	}

	return IntResult(ways(0))
}
