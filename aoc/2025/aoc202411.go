package main

func init() {
	Register(&aoc202411{
		AOC: AOC{
			Day:           11,
			InputFilename: "../2024/input/11.txt",
			Tests: []Test{
				Test{
					Input: `125 17
`,
					Part1: "55312",
					Part2: "",
				},
			},
		},
	})
}

type aoc202411 struct {
	AOC
}

func (aoc *aoc202411) countDigits(n int) int {
	count := 0
	for n > 0 {
		count++
		n /= 10
	}
	return count
}

func (aoc *aoc202411) split(n, ndigits int) (int, int) {
	f := 10
	for ndigits > 2 {
		ndigits -= 2
		f *= 10
	}
	return n / f, n % f
}

func (aoc *aoc202411) blink(counts map[int]int) map[int]int {
	nextCounts := map[int]int{}
	for n, count := range counts {
		if n == 0 {
			nextCounts[1] += count
		} else if ndigits := aoc.countDigits(n); ndigits&1 == 1 {
			nextCounts[n*2024] += count
		} else {
			n1, n2 := aoc.split(n, ndigits)
			nextCounts[n1] += count
			nextCounts[n2] += count
		}
	}
	return nextCounts
}

func (aoc *aoc202411) run(blinks int, input *Input) int {
	counts := map[int]int{}
	for _, n := range input.Ints() {
		counts[n]++
	}

	for _ = range blinks {
		counts = aoc.blink(counts)
	}

	result := 0
	for _, count := range counts {
		result += count
	}
	return result
}

func (aoc *aoc202411) Part1(input *Input) string {
	return IntResult(aoc.run(25, input))
}

func (aoc *aoc202411) Part2(input *Input) string {
	return IntResult(aoc.run(75, input))
}
