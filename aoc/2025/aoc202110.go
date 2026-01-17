package main

import (
	"runtime"
	"slices"
)

func init() {
	Register(&aoc202110{
		AOC: AOC{
			Day:           10,
			InputFilename: "../2021/input/10.txt",
			Tests: []Test{
				Test{
					Input: `[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]
`,
					Part1: "26397",
					Part2: "288957",
				},
			},
		},
	})
}

type aoc202110 struct {
	AOC
}

func (aoc *aoc202110) Part1(input *Input) string {
	type stack struct {
		close rune
		last  *stack
	}

	in := make(chan string)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
		linesLoop:
			for line := range in {
				var st *stack
				for _, b := range line {
					switch b {
					case '(':
						st = &stack{')', st}
					case '[':
						st = &stack{']', st}
					case '{':
						st = &stack{'}', st}
					case '<':
						st = &stack{'>', st}
					case ')':
						if st == nil || st.close != b {
							result += 3
							continue linesLoop
						}
						st = st.last
					case ']':
						if st == nil || st.close != b {
							result += 57
							continue linesLoop
						}
						st = st.last
					case '}':
						if st == nil || st.close != b {
							result += 1197
							continue linesLoop
						}
						st = st.last
					case '>':
						if st == nil || st.close != b {
							result += 25137
							continue linesLoop
						}
						st = st.last
					}
				}
			}
			out <- result
		}()
	}
	for line := range input.Lines() {
		in <- line
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202110) Part2(input *Input) string {
	type stack struct {
		close rune
		last  *stack
	}

	in := make(chan string)
	out := make(chan []int)
	for range runtime.NumCPU() {
		go func() {
			result := []int{}
		linesLoop:
			for line := range in {
				var st *stack
				for _, b := range line {
					switch b {
					case '(':
						st = &stack{')', st}
					case '[':
						st = &stack{']', st}
					case '{':
						st = &stack{'}', st}
					case '<':
						st = &stack{'>', st}
					case ')':
						if st == nil || st.close != b {
							continue linesLoop
						}
						st = st.last
					case ']':
						if st == nil || st.close != b {
							continue linesLoop
						}
						st = st.last
					case '}':
						if st == nil || st.close != b {
							continue linesLoop
						}
						st = st.last
					case '>':
						if st == nil || st.close != b {
							continue linesLoop
						}
						st = st.last
					}
				}
				score := 0
				for st != nil {
					score *= 5
					switch st.close {
					case ')':
						score += 1
					case ']':
						score += 2
					case '}':
						score += 3
					case '>':
						score += 4
					}
					st = st.last
				}
				result = append(result, score)
			}
			out <- result
		}()
	}
	for line := range input.Lines() {
		in <- line
	}
	close(in)
	result := []int{}
	for range runtime.NumCPU() {
		result = append(result, <-out...)
	}
	slices.Sort(result)
	return IntResult(result[len(result)/2])
}
