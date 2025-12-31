package main

import (
	"runtime"
	"slices"
)

func init() {
	Register(&aoc202309{
		AOC: AOC{
			Day:           9,
			InputFilename: "../2023/input/09.txt",
			Tests: []Test{
				Test{
					Input: `0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
`,
					Part1: "114",
					Part2: "2",
				},
			},
		},
	})
}

type aoc202309 struct {
	AOC
}

func (aoc *aoc202309) next(input *Input) ([]int, bool) {
	line, ok := input.Line()
	if !ok {
		return nil, false
	}
	return InputString(line).Ints(), true
}

func (aoc *aoc202309) extrapolate(values []int) int {
	dv := []int{}
	nonzero := false
	val := values[0]
	for _, v := range values[1:] {
		dv = append(dv, v-val)
		if val != v {
			nonzero = true
		}
		val = v
	}
	if nonzero {
		return aoc.extrapolate(dv) + val
	} else {
		return val
	}
}

func (aoc *aoc202309) worker(in chan []int, out chan int) {
	result := 0
	for values := range in {
		result += aoc.extrapolate(values)
	}
	out <- result
}

func (aoc *aoc202309) Part1(input *Input) string {
	in := make(chan []int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go aoc.worker(in, out)
	}
	for values, ok := aoc.next(input); ok; values, ok = aoc.next(input) {
		in <- values
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202309) worker2(in chan []int, out chan int) {
	result := 0
	for values := range in {
		slices.Reverse(values)
		result += aoc.extrapolate(values)
	}
	out <- result
}

func (aoc *aoc202309) Part2(input *Input) string {
	in := make(chan []int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go aoc.worker2(in, out)
	}
	for values, ok := aoc.next(input); ok; values, ok = aoc.next(input) {
		in <- values
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
