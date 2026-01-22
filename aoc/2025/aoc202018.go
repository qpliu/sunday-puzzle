package main

import (
	"runtime"
)

func init() {
	Register(&aoc202018{
		AOC: AOC{
			Day:           18,
			InputFilename: "../2020/input/18.txt",
			Tests: []Test{
				Test{
					Input: `1 + 2 * 3 + 4 * 5 + 6
`,
					Part1: "71",
					Part2: "231",
				},
				Test{
					Input: `1 + (2 * 3) + (4 * (5 + 6))
`,
					Part1: "51",
					Part2: "51",
				},
				Test{
					Input: `2 * 3 + (4 * 5)
`,
					Part1: "26",
					Part2: "46",
				},
				Test{
					Input: `5 + (8 * 3 + 9 + 3 * 4 * 3)
`,
					Part1: "437",
					Part2: "1445",
				},
				Test{
					Input: `5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))
`,
					Part1: "12240",
					Part2: "669060",
				},
				Test{
					Input: `((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2
`,
					Part1: "13632",
					Part2: "23340",
				},
			},
		},
	})
}

type aoc202018 struct {
	AOC
}

func (aoc *aoc202018) eval1(input *Input) int {
	stack := []int{}
	for ch := range input.Chars() {
		switch ch {
		case '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if len(stack) == 0 {
				stack = append(stack, int(ch-'0'))
			} else if stack[len(stack)-1] == -1 {
				stack[len(stack)-2] += int(ch - '0')
				stack = stack[:len(stack)-1]
			} else if stack[len(stack)-1] == -2 {
				stack[len(stack)-2] *= int(ch - '0')
				stack = stack[:len(stack)-1]
			} else if stack[len(stack)-1] == -3 {
				stack = append(stack, int(ch-'0'))
			} else {
				panic("bad input")
			}
		case '+':
			if len(stack) == 0 || stack[len(stack)-1] < 0 {
				panic("bad input")
			}
			stack = append(stack, -1)
		case '*':
			if len(stack) == 0 || stack[len(stack)-1] < 0 {
				panic("bad input")
			}
			stack = append(stack, -2)
		case '(':
			if len(stack) > 0 && stack[len(stack)-1] >= 0 {
				panic("bad input")
			}
			stack = append(stack, -3)
		case ')':
			if len(stack) < 2 || stack[len(stack)-1] < 0 || stack[len(stack)-2] != -3 {
				panic("bad input")
			}
			stack[len(stack)-2] = stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			for len(stack) >= 3 {
				if stack[len(stack)-2] == -1 {
					if stack[len(stack)-3] < 0 {
						panic("?")
					}
					stack[len(stack)-3] += stack[len(stack)-1]
					stack = stack[:len(stack)-2]
				} else if stack[len(stack)-2] == -2 {
					if stack[len(stack)-3] < 0 {
						panic("?")
					}
					stack[len(stack)-3] *= stack[len(stack)-1]
					stack = stack[:len(stack)-2]
				} else {
					break
				}
			}
		}
	}
	if len(stack) != 1 {
		panic("bad input")
	}
	return stack[0]
}

func (aoc *aoc202018) Part1(input *Input) string {
	in := make(chan string)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for line := range in {
				result += aoc.eval1(InputString(line))
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

func (aoc *aoc202018) eval2(input *Input) int {
	stack := []int{}
	for ch := range input.Chars() {
		switch ch {
		case '1', '2', '3', '4', '5', '6', '7', '8', '9':
			if len(stack) == 0 {
				stack = append(stack, int(ch-'0'))
			} else if stack[len(stack)-1] == -1 {
				stack[len(stack)-2] += int(ch - '0')
				stack = stack[:len(stack)-1]
			} else if stack[len(stack)-1] == -2 {
				stack = append(stack, int(ch-'0'))
			} else if stack[len(stack)-1] == -3 {
				stack = append(stack, int(ch-'0'))
			} else {
				panic("bad input")
			}
		case '+':
			if len(stack) == 0 || stack[len(stack)-1] < 0 {
				panic("bad input")
			}
			stack = append(stack, -1)
		case '*':
			if len(stack) == 0 || stack[len(stack)-1] < 0 {
				panic("bad input")
			}
			for len(stack) >= 3 {
				if stack[len(stack)-2] == -1 {
					panic("?")
				} else if stack[len(stack)-2] == -2 {
					if stack[len(stack)-3] < 0 {
						panic("?")
					}
					stack[len(stack)-3] *= stack[len(stack)-1]
					stack = stack[:len(stack)-2]
				} else {
					break
				}
			}
			stack = append(stack, -2)
		case '(':
			if len(stack) > 0 && stack[len(stack)-1] >= 0 {
				panic("bad input")
			}
			stack = append(stack, -3)
		case ')':
			closed := false
			for len(stack) >= 3 {
				if stack[len(stack)-2] == -1 {
					if stack[len(stack)-3] < 0 {
						panic("?")
					}
					stack[len(stack)-3] += stack[len(stack)-1]
					stack = stack[:len(stack)-2]
				} else if stack[len(stack)-2] == -2 {
					if stack[len(stack)-3] < 0 {
						panic("?")
					}
					if closed {
						break
					}
					stack[len(stack)-3] *= stack[len(stack)-1]
					stack = stack[:len(stack)-2]
				} else if stack[len(stack)-2] == -3 {
					if closed {
						break
					} else {
						stack[len(stack)-2] = stack[len(stack)-1]
						stack = stack[:len(stack)-1]
						closed = true
					}
				} else {
					panic("?")
				}
			}
			if !closed {
				if len(stack) < 2 || stack[len(stack)-2] != -3 || stack[len(stack)-1] < 0 {
					panic("?")
				}
				stack[len(stack)-2] = stack[len(stack)-1]
				stack = stack[:len(stack)-1]
			}
		}
	}
	for len(stack) >= 3 {
		if stack[len(stack)-2] == -1 {
			panic("?")
		} else if stack[len(stack)-2] == -2 {
			if stack[len(stack)-3] < 0 {
				panic("?")
			}
			stack[len(stack)-3] *= stack[len(stack)-1]
			stack = stack[:len(stack)-2]
		} else {
			break
		}
	}
	if len(stack) != 1 {
		panic("bad input")
	}
	return stack[0]
}

func (aoc *aoc202018) Part2(input *Input) string {
	in := make(chan string)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for line := range in {
				result += aoc.eval2(InputString(line))
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
