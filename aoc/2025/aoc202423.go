package main

import (
	"bytes"
	"maps"
	"slices"
)

func init() {
	Register(&aoc202423{
		AOC: AOC{
			Day:           23,
			InputFilename: "../2024/input/23.txt",
			Tests: []Test{
				Test{
					Input: `kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
`,
					Part1: "7",
					Part2: "",
				},
				Test{
					Input: `ka-co
ta-co
de-co
ta-ka
de-ta
ka-de`,
					Part1: "",
					Part2: "co,de,ka,ta",
				},
			},
		},
	})
}

type aoc202423 struct {
	AOC
}

func (aoc *aoc202423) connect(graph map[string]map[string]bool, a, b string) {
	acon, ok := graph[a]
	if !ok {
		acon = map[string]bool{}
		graph[a] = acon
	}
	acon[b] = true
}

func (aoc *aoc202423) parse(input *Input) map[string]map[string]bool {
	graph := map[string]map[string]bool{}
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		aoc.connect(graph, line[:2], line[3:])
		aoc.connect(graph, line[3:], line[:2])
	}
	return graph
}

func (aoc *aoc202423) Part1(input *Input) string {
	graph := aoc.parse(input)
	result := 0
	for node, conns := range graph {
		if node[0] != 't' {
			continue
		}
		for a, _ := range conns {
			if a[0] == 't' && a >= node {
				continue
			}
			for b, _ := range conns {
				if b >= a || (b[0] == 't' && b >= node) {
					continue
				}
				if graph[a][b] {
					result++
				}
			}
		}
	}
	return IntResult(result)
}

func (aoc *aoc202423) largest(graph map[string]map[string]bool, threshold int, group map[string]bool, candidates []string) map[string]bool {
	if len(candidates) == 0 || len(candidates)+len(group) <= threshold {
		return group
	}
	group1 := aoc.largest(graph, threshold, group, candidates[1:])
	conns := graph[candidates[0]]
	for n := range group {
		if !conns[n] {
			return group1
		}
	}
	group2 := maps.Clone(group)
	group2[candidates[0]] = true
	group2 = aoc.largest(graph, threshold, group2, candidates[1:])
	if len(group1) < len(group2) {
		return group2
	}
	return group1
}

func (aoc *aoc202423) Part2(input *Input) string {
	graph := aoc.parse(input)
	largestGroup := map[string]bool{}
	for node, conns := range graph {
		if len(conns) < len(largestGroup) { // max size is len(conns)+1
			continue
		}
		candidates := make([]string, len(conns))
		for n := range conns {
			candidates = append(candidates, n)
		}
		group := aoc.largest(graph, len(largestGroup), map[string]bool{node: true}, candidates)
		if len(group) > len(largestGroup) {
			largestGroup = group
		}
	}
	nodes := []string{}
	for node := range largestGroup {
		nodes = append(nodes, node)
	}
	slices.Sort(nodes)
	buf := &bytes.Buffer{}
	for _, node := range nodes {
		buf.WriteByte(',')
		buf.WriteString(node)
	}
	buf.ReadByte()
	return buf.String()
}
