package main

func init() {
	Register(&aoc202002{
		AOC: AOC{
			Day:           2,
			InputFilename: "../2020/input/02.txt",
			Tests: []Test{
				Test{
					Input: `1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
`,
					Part1: "2",
					Part2: "1",
				},
			},
		},
	})
}

type aoc202002 struct {
	AOC
}

func (aoc *aoc202002) Part1(input *Input) string {
	result := 0
	for {
		n1, ok := input.Int()
		if !ok {
			break
		}
		n2, _ := input.Int()
		n2 = -n2
		w, _ := input.Word()
		pw, _ := input.Word()
		n := 0
		for i := range pw {
			if pw[i] == w[0] {
				n++
			}
		}
		if n >= n1 && n <= n2 {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202002) Part2(input *Input) string {
	result := 0
	for {
		n1, ok := input.Int()
		if !ok {
			break
		}
		n2, _ := input.Int()
		n2 = -n2
		w, _ := input.Word()
		pw, _ := input.Word()
		if (pw[n1-1] == w[0]) != (pw[n2-1] == w[0]) {
			result++
		}
	}
	return IntResult(result)
}
