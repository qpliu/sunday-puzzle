package main

import (
	"runtime"
)

func init() {
	Register(&aoc202312{
		AOC: AOC{
			Day:           12,
			InputFilename: "../2023/input/12.txt",
			Tests: []Test{
				Test{
					Input: `???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
`,
					Part1: "21",
					Part2: "525152",
				},
			},
		},
	})
}

type aoc202312 struct {
	AOC
}

func (aoc *aoc202312) next(part1 bool, input *Input) (struct {
	row    string
	groups []int
}, bool) {
	line, ok := input.Line()
	if !ok {
		return struct {
			row    string
			groups []int
		}{}, false
	}
	input = InputString(line)
	row, _ := input.Word()
	groups := input.Ints()
	if !part1 {
		row = row + "?" + row + "?" + row + "?" + row + "?" + row
		groups5 := []int{}
		for range 5 {
			for _, group := range groups {
				groups5 = append(groups5, group)
			}
		}
		groups = groups5
	}
	return struct {
		row    string
		groups []int
	}{row, groups}, true
}

func (aoc *aoc202312) ways(r struct {
	row    string
	groups []int
}, index [3]int, memo map[[3]int]int) int {
	if n, ok := memo[index]; ok {
		return n
	}
	if index[0] == 1 {
		if index[1] >= len(r.row) {
			if index[2] >= len(r.groups) {
				memo[index] = 1
				return 1
			}
			memo[index] = 0
			return 0
		}
		switch r.row[index[1]] {
		case '.', '?':
			n := aoc.ways(r, [3]int{0, index[1] + 1, index[2]}, memo)
			memo[index] = n
			return n
		case '#':
			memo[index] = 0
			return 0
		default:
			panic("bad input")
		}
	}
	if index[0] > 0 {
		if index[1] >= len(r.row) || r.row[index[1]] == '.' {
			memo[index] = 0
			return 0
		}
		n := aoc.ways(r, [3]int{index[0] - 1, index[1] + 1, index[2]}, memo)
		memo[index] = n
		return n
	}
	if index[1] >= len(r.row) {
		if index[2] < len(r.groups) {
			memo[index] = 0
			return 0
		}
		memo[index] = 1
		return 1
	}
	switch r.row[index[1]] {
	case '.':
		n := aoc.ways(r, [3]int{0, index[1] + 1, index[2]}, memo)
		memo[index] = n
		return n
	case '#':
		if index[2] >= len(r.groups) {
			memo[index] = 0
			return 0
		} else {
			n := aoc.ways(r, [3]int{r.groups[index[2]], index[1] + 1, index[2] + 1}, memo)
			memo[index] = n
			return n
		}
	case '?':
		n := aoc.ways(r, [3]int{0, index[1] + 1, index[2]}, memo)
		if index[2] < len(r.groups) {
			n += aoc.ways(r, [3]int{r.groups[index[2]], index[1] + 1, index[2] + 1}, memo)
		}
		memo[index] = n
		return n
	default:
		panic("bad input")
	}
}

func (aoc *aoc202312) result(part1 bool, input *Input) string {
	in := make(chan struct {
		row    string
		groups []int
	})
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for r := range in {
				result += aoc.ways(r, [3]int{}, map[[3]int]int{})
			}
			out <- result
		}()
	}
	for r, ok := aoc.next(part1, input); ok; r, ok = aoc.next(part1, input) {
		in <- r
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202312) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202312) Part2(input *Input) string {
	return aoc.result(false, input)
}
