package main

import (
	"bytes"
)

func init() {
	Register(&aoc202319{
		AOC: AOC{
			Day:           19,
			InputFilename: "../2023/input/19.txt",
			Tests: []Test{
				Test{
					Input: `px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
`,
					Part1: "19114",
					Part2: "167409079868000",
				},
			},
		},
	})
}

type aoc202319 struct {
	AOC
}

type aoc202319_workflow struct {
	xmas int
	gt   bool
	n    int

	trueName string
	trueFlow *aoc202319_workflow
	next     *aoc202319_workflow
}

func (aoc *aoc202319) parse(input *Input) *aoc202319_workflow {
	paragraph, _ := input.Paragraph()
	input = InputString(paragraph)

	workflows := map[string]*aoc202319_workflow{}
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		inp := InputString(line)
		wf := &aoc202319_workflow{}
		workflows[aoc.name(inp)] = wf
		for {
			if inp.Skip("x>") {
				wf.xmas = 0
				wf.gt = true
			} else if inp.Skip("x<") {
				wf.xmas = 0
				wf.gt = false
			} else if inp.Skip("m>") {
				wf.xmas = 1
				wf.gt = true
			} else if inp.Skip("m<") {
				wf.xmas = 1
				wf.gt = false
			} else if inp.Skip("a>") {
				wf.xmas = 2
				wf.gt = true
			} else if inp.Skip("a<") {
				wf.xmas = 2
				wf.gt = false
			} else if inp.Skip("s>") {
				wf.xmas = 3
				wf.gt = true
			} else if inp.Skip("s<") {
				wf.xmas = 3
				wf.gt = false
			} else {
				wf.xmas = -1
			}
			if wf.xmas >= 0 {
				wf.n, _ = inp.Int()
				inp.Skip(":")
			}
			wf.trueName = aoc.name(inp)
			if _, ok := inp.Peek(); !ok {
				break
			}
			wf.next = &aoc202319_workflow{}
			wf = wf.next
		}
	}

	for _, wf := range workflows {
		for wf != nil {
			wf.trueFlow = workflows[wf.trueName]
			wf = wf.next
		}
	}

	return workflows["in"]
}

func (aoc *aoc202319) name(input *Input) string {
	buf := bytes.Buffer{}
	for {
		ch, _ := input.Char()
		switch ch {
		case 0, '{', '}', ',':
			return buf.String()
		default:
			buf.WriteByte(ch)
		}
	}
}

func (aoc *aoc202319) next(input *Input) ([4]int, bool) {
	x, _ := input.Int()
	m, _ := input.Int()
	a, _ := input.Int()
	s, ok := input.Int()
	return [4]int{x, m, a, s}, ok
}

func (aoc *aoc202319) Part1(input *Input) string {
	in := aoc.parse(input)

	result := 0
	for part, ok := aoc.next(input); ok; part, ok = aoc.next(input) {
		wf := in
		dest := ""
		for wf != nil {
			dest = wf.trueName
			if wf.xmas < 0 {
				wf = wf.trueFlow
			} else if wf.gt && part[wf.xmas] > wf.n {
				wf = wf.trueFlow
			} else if !wf.gt && part[wf.xmas] < wf.n {
				wf = wf.trueFlow
			} else {
				wf = wf.next
			}
		}
		if dest == "A" {
			result += part[0] + part[1] + part[2] + part[3]
		}
	}
	return IntResult(result)
}

func (aoc *aoc202319) Part2(input *Input) string {
	in := aoc.parse(input)

	type pending struct {
		lo, hi [4]int
		wf     *aoc202319_workflow
		dest   string
	}

	queue := NewQueue[pending]()
	parts := pending{
		lo: [4]int{1, 1, 1, 1},
		hi: [4]int{4000, 4000, 4000, 4000},
		wf: in,
	}

	result := 0
	for {
		if parts.wf == nil {
			if parts.dest == "A" {
				result += (1 + parts.hi[0] - parts.lo[0]) * (1 + parts.hi[1] - parts.lo[1]) * (1 + parts.hi[2] - parts.lo[2]) * (1 + parts.hi[3] - parts.lo[3])
			}
			if queue.Empty() {
				break
			}
			parts = queue.Dequeue()
			continue
		}
		if parts.wf.xmas < 0 {
			parts.dest = parts.wf.trueName
			parts.wf = parts.wf.trueFlow
			continue
		}
		if parts.wf.gt {
			if parts.lo[parts.wf.xmas] > parts.wf.n {
				parts.dest = parts.wf.trueName
				parts.wf = parts.wf.trueFlow
				continue
			}
			if parts.hi[parts.wf.xmas] <= parts.wf.n {
				parts.dest = parts.wf.trueName
				parts.wf = parts.wf.next
				continue
			}
			lo := parts.lo
			lo[parts.wf.xmas] = parts.wf.n + 1
			queue.Enqueue(pending{
				lo:   lo,
				hi:   parts.hi,
				wf:   parts.wf.trueFlow,
				dest: parts.wf.trueName,
			})
			parts.hi[parts.wf.xmas] = parts.wf.n
			parts.dest = parts.wf.trueName
			parts.wf = parts.wf.next
		} else {
			if parts.lo[parts.wf.xmas] >= parts.wf.n {
				parts.dest = parts.wf.trueName
				parts.wf = parts.wf.next
				continue
			}
			if parts.hi[parts.wf.xmas] < parts.wf.n {
				parts.dest = parts.wf.trueName
				parts.wf = parts.wf.trueFlow
				continue
			}
			hi := parts.hi
			hi[parts.wf.xmas] = parts.wf.n - 1
			queue.Enqueue(pending{
				lo:   parts.lo,
				hi:   hi,
				wf:   parts.wf.trueFlow,
				dest: parts.wf.trueName,
			})
			parts.lo[parts.wf.xmas] = parts.wf.n
			parts.dest = parts.wf.trueName
			parts.wf = parts.wf.next
		}
	}
	return IntResult(result)
}
