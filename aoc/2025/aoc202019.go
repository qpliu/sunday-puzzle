package main

import (
	"runtime"
)

func init() {
	Register(&aoc202019{
		AOC: AOC{
			Day:           19,
			InputFilename: "../2020/input/19.txt",
			Tests: []Test{
				Test{
					Input: `0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb
`,
					Part1: "2",
					Part2: "",
				},
				Test{
					Input: `42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
`,
					Part1: "3",
					Part2: "12",
				},
			},
		},
	})
}

type aoc202019 struct {
	AOC
}

func (aoc *aoc202019) rules(input *Input) (int, int, [][][]int) {
	a := 0
	b := 0
	rules := make([][][]int, 150)
	for {
		line, _ := input.Line()
		if line == "" {
			return a, b, rules
		}
		in := InputString(line)
		n, _ := in.Int()
		in.Skip(": ")
		if in.LookingAt("\"a\"") {
			a = n
			continue
		} else if in.LookingAt("\"b\"") {
			b = n
			continue
		}
		r := []int{}
		for {
			if in.Skip(" |") {
				rules[n] = append(rules[n], r)
				r = []int{}
			}
			i, ok := in.Int()
			if ok {
				r = append(r, i)
			} else {
				rules[n] = append(rules[n], r)
				break
			}
		}
	}
}

func (aoc *aoc202019) valid1(a, b int, rules [][][]int, str string) bool {
	var match func(int, int) (int, bool)
	match = func(i, ruleIndex int) (int, bool) {
		if i >= len(str) {
			return 0, false
		} else if ruleIndex == a {
			if str[i] == 'a' {
				return i + 1, true
			} else {
				return 0, false
			}
		} else if ruleIndex == b {
			if str[i] == 'b' {
				return i + 1, true
			} else {
				return 0, false
			}
		}
	alternativeLoop:
		for _, rule := range rules[ruleIndex] {
			j := i
			for _, r := range rule {
				k, ok := match(j, r)
				if !ok {
					continue alternativeLoop
				}
				j = k
			}
			return j, true
		}
		return 0, false
	}

	i, ok := match(0, 0)
	return ok && i == len(str)
}

func (aoc *aoc202019) Part1(input *Input) string {
	a, b, rules := aoc.rules(input)
	in := make(chan string)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for str := range in {
				if aoc.valid1(a, b, rules, str) {
					result++
				}
			}
			out <- result
		}()
	}
	for str := range input.Lines() {
		in <- str
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202019) valid2(a, b int, rules [][][]int, str string) bool {
	var match func(int, int) map[int]bool
	match = func(i, ruleIndex int) map[int]bool {
		if i >= len(str) {
			return map[int]bool{}
		} else if ruleIndex == a {
			if str[i] == 'a' {
				return map[int]bool{i + 1: true}
			} else {
				return map[int]bool{}
			}
		} else if ruleIndex == b {
			if str[i] == 'b' {
				return map[int]bool{i + 1: true}
			} else {
				return map[int]bool{}
			}
		} else if ruleIndex == 8 {
			matches := map[int]bool{}
			queue := NewQueue[int]()
			queue.Enqueue(i)
			for !queue.Empty() {
				for j := range match(queue.Dequeue(), 42) {
					matches[j] = true
					queue.Enqueue(j)
				}
			}
			return matches
		} else if ruleIndex == 11 {
			queue := NewQueue[[3]int]()
			// if e[0] == 0, matching opening 42s
			//    otherwise, matching closing 31s
			// e[1] is number of closing 31s needed
			// e[2] is index of end of current match
			for j := range match(i, 42) {
				queue.Enqueue([3]int{0, 1, j})
			}
			matches := map[int]bool{}
			for !queue.Empty() {
				entry := queue.Dequeue()
				if entry[0] == 0 {
					for j := range match(entry[2], 42) {
						queue.Enqueue([3]int{0, entry[1] + 1, j})
					}
				}
				for j := range match(entry[2], 31) {
					if entry[1] == 1 {
						matches[j] = true
					} else {
						queue.Enqueue([3]int{1, entry[1] - 1, j})
					}
				}
			}
			return matches
		}
		matches := map[int]bool{}
		for _, rule := range rules[ruleIndex] {
			queue := NewQueue[int]()
			queue.Enqueue(i)
			nextQueue := NewQueue[int]()
			for _, r := range rule {
				for !queue.Empty() {
					for k := range match(queue.Dequeue(), r) {
						nextQueue.Enqueue(k)
					}
				}
				queue, nextQueue = nextQueue, queue
			}
			for !queue.Empty() {
				matches[queue.Dequeue()] = true
			}
		}
		return matches
	}

	return match(0, 0)[len(str)]
}

func (aoc *aoc202019) Part2(input *Input) string {
	a, b, rules := aoc.rules(input)
	in := make(chan string)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for str := range in {
				if aoc.valid2(a, b, rules, str) {
					result++
				}
			}
			out <- result
		}()
	}
	for str := range input.Lines() {
		in <- str
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
