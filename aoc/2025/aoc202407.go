package main

import (
	"runtime"
)

func init() {
	Register(&aoc202407{
		AOC: AOC{
			Day:           7,
			InputFilename: "../2024/input/07.txt",
			Tests: []Test{
				Test{
					Input: `190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
`,
					Part1: "3749",
					Part2: "11387",
				},
			},
		},
	})
}

type aoc202407 struct {
	AOC
}

func (aoc *aoc202407) next(input *Input) ([]int, bool) {
	line, ok := input.Line()
	if !ok {
		return nil, false
	}
	return InputString(line).Ints(), true
}

func (aoc *aoc202407) worker(in chan []int, out chan int, test func(int, int, []int) bool) {
	go func() {
		total := 0
		for {
			eqn, ok := <-in
			if !ok {
				out <- total
				return
			}
			if test(eqn[0], eqn[1], eqn[2:]) {
				total += eqn[0]
			}
		}
	}()
}

func (aoc *aoc202407) valid1(value int, term int, eqn []int) bool {
	if term > value {
		return false
	}
	if len(eqn) == 0 {
		return term == value
	}
	if aoc.valid1(value, term+eqn[0], eqn[1:]) {
		return true
	}
	if aoc.valid1(value, term*eqn[0], eqn[1:]) {
		return true
	}
	return false
}

func (aoc *aoc202407) Part1(input *Input) string {
	in := make(chan []int)
	out := make(chan int)

	for _ = range runtime.NumCPU() {
		aoc.worker(in, out, func(value int, term int, eqn []int) bool {
			return aoc.valid1(value, term, eqn)
		})
	}
	for eqn, ok := aoc.next(input); ok; eqn, ok = aoc.next(input) {
		in <- eqn
	}
	close(in)
	result := 0
	for _ = range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202407) valid2(value int, term int, eqn []int) bool {
	if term > value {
		return false
	}
	if len(eqn) == 0 {
		return term == value
	}
	if aoc.valid2(value, term+eqn[0], eqn[1:]) {
		return true
	}
	if aoc.valid2(value, term*eqn[0], eqn[1:]) {
		return true
	}
	e0 := eqn[0]
	f := 10
	for e0 >= 10 {
		f *= 10
		e0 /= 10
	}
	if aoc.valid2(value, f*term+eqn[0], eqn[1:]) {
		return true
	}
	return false
}

func (aoc aoc202407) Part2(input *Input) string {
	in := make(chan []int)
	out := make(chan int)

	for _ = range runtime.NumCPU() {
		aoc.worker(in, out, func(value int, term int, eqn []int) bool {
			return aoc.valid2(value, term, eqn)
		})
	}
	for eqn, ok := aoc.next(input); ok; eqn, ok = aoc.next(input) {
		in <- eqn
	}
	close(in)
	result := 0
	for _ = range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
