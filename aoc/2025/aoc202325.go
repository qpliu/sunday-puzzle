package main

import (
	"math/rand"
	"runtime"
	"slices"
	"sync"
)

func init() {
	Register(&aoc202325{
		AOC: AOC{
			Day:           25,
			InputFilename: "../2023/input/25.txt",
			Tests: []Test{
				Test{
					Input: `jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr
`,
					Part1: "54",
					Part2: "",
				},
			},
		},
	})
}

type aoc202325 struct {
	AOC
}

func (aoc *aoc202325) parse(input *Input) [][2]int32 {
	edges := [][2]int32{}
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		inp := InputString(line)
		n1, _ := inp.Word()
		i1 := int32(n1[0])<<16 + int32(n1[1])<<8 + int32(n1[2])
		for n2, ok := inp.Word(); ok; n2, ok = inp.Word() {
			i2 := int32(n2[0])<<16 + int32(n2[1])<<8 + int32(n2[2])
			edges = append(edges, [2]int32{i1, i2})
		}
	}
	return edges
}

func (aoc *aoc202325) karger(edges [][2]int32) (int, int) {
	contractions := map[int32]int32{}
	group := func(node int32) int32 {
		for n, ok := contractions[node]; ok; n, ok = contractions[n] {
			node = n
		}
		return node
	}

	groups := map[int32]int{}
	for _, edge := range edges {
		groups[edge[0]] = 1
		groups[edge[1]] = 1
	}

	for _, edge := range edges {
		n1 := group(edge[0])
		n2 := group(edge[1])
		if n1 == n2 {
			continue
		}
		s1 := groups[n1]
		s2 := groups[n2]
		if s1 > s2 {
			contractions[n2] = n1
			groups[n1] = s1 + s2
			delete(groups, n2)
		} else {
			contractions[n1] = n2
			groups[n2] = s1 + s2
			delete(groups, n1)
		}
		if len(groups) <= 2 {
			break
		}
	}
	if len(groups) != 2 {
		return 0, 0
	}

	surviving := 0
	for _, edge := range edges {
		n1 := group(edge[0])
		n2 := group(edge[1])
		if n1 != n2 {
			surviving++
		}
	}

	result := 1
	for _, s := range groups {
		result *= s
	}
	return result, surviving
}

func (aoc *aoc202325) Part1(input *Input) string {
	edges := aoc.parse(input)
	out := make(chan int)

	done := false
	lock := sync.Mutex{}
	for cpu := range runtime.NumCPU() {
		go func(edges [][2]int32, r *rand.Rand) {
			result := 0
			for {
				for range 5 {
					r.Shuffle(len(edges), func(i, j int) {
						edges[i], edges[j] = edges[j], edges[i]
					})
					res, surviving := aoc.karger(slices.Clone(edges))
					if surviving == 3 {
						result = res
						lock.Lock()
						done = true
						lock.Unlock()
						break
					}
				}
				lock.Lock()
				d := done
				lock.Unlock()
				if d {
					break
				}
			}
			out <- result
		}(slices.Clone(edges), rand.New(rand.NewSource(202325+int64(cpu))))
	}

	result := 0
	for range runtime.NumCPU() {
		result = max(result, <-out)
	}
	return IntResult(result)
}

func (aoc *aoc202325) Part2(input *Input) string {
	return ""
}
