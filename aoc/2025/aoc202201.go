package main

func init() {
	Register(&aoc202201{
		AOC: AOC{
			Day:           1,
			InputFilename: "../2022/input/01.txt",
			Tests: []Test{
				Test{
					Input: `1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
`,
					Part1: "24000",
					Part2: "45000",
				},
			},
		},
	})
}

type aoc202201 struct {
	AOC
}

func (aoc *aoc202201) sums(input *Input) Seq[int] {
	return func(yield func(int) bool) {
		for paragraph := range input.Paragraphs() {
			total := 0
			for _, cal := range InputString(paragraph).Ints() {
				total += cal
			}
			if !yield(total) {
				return
			}
		}
	}
}

func (aoc *aoc202201) Part1(input *Input) string {
	result := 0
	for cals := range aoc.sums(input) {
		result = max(result, cals)
	}
	return IntResult(result)
}

func (aoc *aoc202201) Part2(input *Input) string {
	r1, r2, r3 := 0, 0, 0
	for cals := range aoc.sums(input) {
		if cals > r1 {
			r1, cals = cals, r1
		}
		if cals > r2 {
			r2, cals = cals, r2
		}
		if cals > r3 {
			r3 = cals
		}
	}
	return IntResult(r1 + r2 + r3)
}
