package main

import (
	"math/bits"
	"runtime"
)

func init() {
	Register(&aoc202216{
		AOC: AOC{
			Day:           16,
			InputFilename: "../2022/input/16.txt",
			Tests: []Test{
				Test{
					Input: `Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
`,
					Part1: "1651",
					Part2: "1707",
				},
			},
		},
	})
}

type aoc202216 struct {
	AOC
}

type aoc202216_valve struct {
	name    string
	index   int
	flow    int
	tunnels []aoc202216_tunnel

	tunnelNames []string
}

type aoc202216_tunnel struct {
	dist  int
	valve *aoc202216_valve
}

func (aoc *aoc202216) parse(input *Input) (*aoc202216_valve, []*aoc202216_valve) {
	valves := map[string]*aoc202216_valve{}
	targetValves := []*aoc202216_valve{}
	for {
		if _, ok := input.Word(); !ok {
			break
		}
		name, _ := input.Word()
		flow, _ := input.Int()
		input.Skip(";")
		input.Word()
		input.Word()
		input.Word()
		input.Word()
		tunnelNames := []string{}
		for {
			dest, _ := input.Word()
			tunnelNames = append(tunnelNames, dest[:2])
			if len(dest) == 2 {
				break
			}
		}
		valve := &aoc202216_valve{
			name: name,
			flow: flow,

			tunnelNames: tunnelNames,
		}
		valves[name] = valve
		if flow > 0 {
			valve.index = len(targetValves)
			targetValves = append(targetValves, valve)
		}
	}

	for _, valve := range valves {
		visited := map[*aoc202216_valve]int{}
		queue := NewQueue[aoc202216_tunnel]()
		for _, tunnelName := range valve.tunnelNames {
			queue.Enqueue(aoc202216_tunnel{1, valves[tunnelName]})
		}
		for !queue.Empty() {
			tunnel := queue.Dequeue()
			if tunnel.valve == valve {
				continue
			}
			dist, ok := visited[tunnel.valve]
			if ok && dist <= tunnel.dist {
				continue
			}
			visited[tunnel.valve] = tunnel.dist
			for _, tunnelName := range tunnel.valve.tunnelNames {
				queue.Enqueue(aoc202216_tunnel{tunnel.dist + 1, valves[tunnelName]})
			}
		}

		for dest, dist := range visited {
			if dest.flow > 0 {
				valve.tunnels = append(valve.tunnels, aoc202216_tunnel{dist, dest})
			}
		}
	}
	return valves["AA"], targetValves
}

func (aoc *aoc202216) search(opened BitSet64, timeLeft int, aa *aoc202216_valve, targetValves []*aoc202216_valve) int {
	type st struct {
		opened BitSet64
		valve  *aoc202216_valve
	}

	type path struct {
		state    st
		timeLeft int
		released int
	}

	start := []path{}
	for _, tunnel := range aa.tunnels {
		if opened.Contains(tunnel.valve.index) || timeLeft-tunnel.dist <= 0 {
			continue
		}
		start = append(start, path{
			state: st{
				opened: opened.Add(tunnel.valve.index),
				valve:  tunnel.valve,
			},
			timeLeft: timeLeft - tunnel.dist - 1,
			released: (timeLeft - tunnel.dist - 1) * tunnel.valve.flow,
		})
	}

	done := func(p path) bool {
		for _, tunnel := range p.state.valve.tunnels {
			if tunnel.dist+1 < p.timeLeft && !p.state.opened.Contains(tunnel.valve.index) {
				return false
			}
		}
		return true
	}

	priority := func(p path) int {
		priority := p.released
		for _, tunnel := range p.state.valve.tunnels {
			if tunnel.dist+1 < p.timeLeft && !p.state.opened.Contains(tunnel.valve.index) {
				priority += (p.timeLeft - tunnel.dist - 1) * tunnel.valve.flow
			}
		}
		return priority
	}

	state := func(p path) st {
		return p.state
	}

	neighbors := func(p path) []path {
		n := []path{}
		for _, tunnel := range p.state.valve.tunnels {
			if tunnel.dist+1 < p.timeLeft && !p.state.opened.Contains(tunnel.valve.index) {
				n = append(n, path{
					state: st{
						opened: p.state.opened.Add(tunnel.valve.index),
						valve:  tunnel.valve,
					},
					timeLeft: p.timeLeft - tunnel.dist - 1,
					released: p.released + (p.timeLeft-tunnel.dist-1)*tunnel.valve.flow,
				})
			}
		}
		return n
	}

	p, _ := AstarSearch(start, done, priority, state, neighbors)
	return p.released
}

func (aoc *aoc202216) Part1(input *Input) string {
	aa, targetValves := aoc.parse(input)
	return IntResult(aoc.search(BitSet64(0), 30, aa, targetValves))
}

func (aoc *aoc202216) Part2(input *Input) string {
	aa, targetValves := aoc.parse(input)
	mask := BitSet64(1<<len(targetValves) - 1)
	in := make(chan BitSet64)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for opened := range in {
				r := aoc.search(opened, 26, aa, targetValves)
				if r > 0 {
					r += aoc.search(mask^opened, 26, aa, targetValves)
				}
				result = max(result, r)
			}
			out <- result
		}()
	}
	for i := range mask>>1 + 1 {
		a := bits.OnesCount64(uint64(i))
		b := bits.OnesCount64(uint64(i ^ mask))
		if max(a-b, b-a) < 2 {
			in <- BitSet64(i)
		}
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result = max(result, <-out)
	}
	return IntResult(result)
}
