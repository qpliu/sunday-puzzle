package main

func init() {
	Register(&aoc202023{
		AOC: AOC{
			Day:           23,
			InputFilename: "../2020/input/23.txt",
			Tests: []Test{
				Test{
					Input: `389125467
`,
					Part1: "67384529",
					Part2: "149245887792",
				},
			},
		},
	})
}

type aoc202023 struct {
	AOC
}

func (aoc *aoc202023) parse(next, prev []int, input *Input) int {
	for i := range next {
		next[i] = i + 1
		prev[i] = i - 1
	}
	next[len(next)-1] = 0
	prev[0] = len(next) - 1
	last := 0
	for ch := range input.Chars() {
		if ch < '0' || ch > '9' {
			break
		}
		cur := int(ch - '0')
		next[prev[cur]] = next[cur]
		prev[next[cur]] = prev[cur]
		prev[cur] = last
		next[cur] = next[last]
		next[last] = cur
		prev[next[cur]] = cur
		last = cur
	}
	next[prev[0]] = next[0]
	prev[next[0]] = prev[0]
	return next[0]
}

func (aoc *aoc202023) move(cur int, next, prev []int) int {
	c1 := next[cur]
	c2 := next[c1]
	c3 := next[c2]
	dest := cur - 1
	if dest == 0 {
		dest = len(next) - 1
	}
	for dest == c1 || dest == c2 || dest == c3 {
		dest--
		if dest == 0 {
			dest = len(next) - 1
		}
	}
	next[cur] = next[c3]
	prev[next[c3]] = cur
	next[c3] = next[dest]
	prev[next[dest]] = c3
	next[dest] = c1
	prev[c1] = dest
	return next[cur]
}

func (aoc *aoc202023) Part1(input *Input) string {
	next, prev := make([]int, 10), make([]int, 10)
	cur := aoc.parse(next, prev, input)
	for range 100 {
		cur = aoc.move(cur, next, prev)
	}
	result := []byte{}
	cur = next[1]
	for cur != 1 {
		result = append(result, byte('0'+cur))
		cur = next[cur]
	}
	return string(result)
}

func (aoc *aoc202023) Part2(input *Input) string {
	next, prev := make([]int, 1000001), make([]int, 1000001)
	cur := aoc.parse(next, prev, input)
	for range 10000000 {
		cur = aoc.move(cur, next, prev)
	}
	return IntResult(next[1] * next[next[1]])
}
