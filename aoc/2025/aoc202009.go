package main

func init() {
	Register(&aoc202009{
		AOC: AOC{
			Day:           9,
			InputFilename: "../2020/input/09.txt",
			Tests: []Test{
				Test{
					Input: `35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
`,
					Part1: "127",
					Part2: "62",
				},
			},
		},
	})
}

type aoc202009 struct {
	AOC
}

func (aoc *aoc202009) invalid(preamble int, numbers []int) int {
	i := preamble - 1
loop:
	for {
		i++
		n := numbers[i]
		for j := range preamble {
			m := n - numbers[i-1-j]
			for k := range j {
				if m == numbers[i-1-k] {
					continue loop
				}
			}
		}
		return n
	}
}

func (aoc *aoc202009) Test1(input *Input) string {
	return IntResult(aoc.invalid(5, input.Ints()))
}

func (aoc *aoc202009) Part1(input *Input) string {
	return IntResult(aoc.invalid(25, input.Ints()))
}

func (aoc *aoc202009) weakness(invalid int, numbers []int) int {
	i1 := 0
	i2 := 1
	sum := numbers[i1] + numbers[i2]
	for {
		// there are no negative numbers in the example or in my input
		if sum < invalid {
			i2++
			sum += numbers[i2]
		} else if sum > invalid {
			sum -= numbers[i1]
			i1++
		} else {
			smallest := invalid
			largest := 0
			for i := i1; i <= i2; i++ {
				smallest = min(smallest, numbers[i])
				largest = max(largest, numbers[i])
			}
			return smallest + largest
		}
	}
}

func (aoc *aoc202009) Test2(input *Input) string {
	numbers := input.Ints()
	return IntResult(aoc.weakness(aoc.invalid(5, numbers), numbers))
}

func (aoc *aoc202009) Part2(input *Input) string {
	numbers := input.Ints()
	return IntResult(aoc.weakness(aoc.invalid(25, numbers), numbers))
}
