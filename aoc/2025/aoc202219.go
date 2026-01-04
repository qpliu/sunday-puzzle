package main

import (
	"runtime"
)

func init() {
	Register(&aoc202219{
		AOC: AOC{
			Day:           19,
			InputFilename: "../2022/input/19.txt",
			Tests: []Test{
				Test{
					Input: `Blueprint 1:
  Each ore robot costs 4 ore.
  Each clay robot costs 2 ore.
  Each obsidian robot costs 3 ore and 14 clay.
  Each geode robot costs 2 ore and 7 obsidian.

Blueprint 2:
  Each ore robot costs 2 ore.
  Each clay robot costs 3 ore.
  Each obsidian robot costs 3 ore and 8 clay.
  Each geode robot costs 3 ore and 12 obsidian.
`,
					Part1: "33",
					Part2: "3472",
				},
			},
		},
	})
}

type aoc202219 struct {
	AOC
}

func (aoc *aoc202219) blueprint(input *Input) Seq[[7]int] {
	return func(yield func([7]int) bool) {
		for {
			id, _ := input.Int()
			oreOre, _ := input.Int()
			clyOre, _ := input.Int()
			obsOre, _ := input.Int()
			obsCly, _ := input.Int()
			geoOre, _ := input.Int()
			geoObs, ok := input.Int()
			if !ok || !yield([7]int{
				id,
				oreOre,
				clyOre,
				obsOre,
				obsCly,
				geoOre,
				geoObs,
			}) {
				return
			}
		}
	}
}

func (aoc *aoc202219) search(blueprint [7]int, timeLeft int) int {
	oreOre := blueprint[1]
	clyOre := blueprint[2]
	obsOre := blueprint[3]
	obsCly := blueprint[4]
	geoOre := blueprint[5]
	geoObs := blueprint[6]

	type state struct {
		ore, oreBots int
		cly, clyBots int
		obs, obsBots int
		geo, geoBots int
		timeLeft     int
	}

	queue := NewPriorityQueue[*state](func(st *state) int {
		return st.geo + st.timeLeft*st.geoBots + st.timeLeft*(st.timeLeft+2)/2
	})
	queue.Push(&state{oreBots: 1, timeLeft: timeLeft})

	for {
		st, ok := queue.Pop()
		if !ok {
			panic("?")
		}

		if st.timeLeft == 0 {
			return st.geo
		}
		wait := true

		maxOreBots := geoOre
		if st.clyBots < obsCly {
			maxOreBots = max(maxOreBots, clyOre)
		}
		if st.obsBots < geoObs {
			maxOreBots = max(maxOreBots, obsOre)
		}

		dtGeo := timeLeft
		if st.obsBots > 0 {
			dt := max((geoOre-st.ore+st.oreBots-1)/st.oreBots, (geoObs-st.obs+st.obsBots-1)/st.obsBots, 0) + 1
			if dt < st.timeLeft {
				queue.Push(&state{
					ore:      st.ore + dt*st.oreBots - geoOre,
					oreBots:  st.oreBots,
					cly:      st.cly + dt*st.clyBots,
					clyBots:  st.clyBots,
					obs:      st.obs + dt*st.obsBots - geoObs,
					obsBots:  st.obsBots,
					geo:      st.geo + dt*st.geoBots,
					geoBots:  st.geoBots + 1,
					timeLeft: st.timeLeft - dt,
				})
				dtGeo = dt
				wait = false
			}
		}

		if st.clyBots > 0 && st.obsBots < geoObs {
			dt := max((obsOre-st.ore+st.oreBots-1)/st.oreBots, (obsCly-st.cly+st.clyBots-1)/st.clyBots, 0) + 1
			if dt < st.timeLeft && dt < dtGeo {
				queue.Push(&state{
					ore:      st.ore + dt*st.oreBots - obsOre,
					oreBots:  st.oreBots,
					cly:      st.cly + dt*st.clyBots - obsCly,
					clyBots:  st.clyBots,
					obs:      st.obs + dt*st.obsBots,
					obsBots:  st.obsBots + 1,
					geo:      st.geo + dt*st.geoBots,
					geoBots:  st.geoBots,
					timeLeft: st.timeLeft - dt,
				})
				wait = false
			}
		}

		if st.oreBots < maxOreBots {
			dt := max((oreOre-st.ore+st.oreBots-1)/st.oreBots, 0) + 1
			if dt < st.timeLeft && dt < dtGeo {
				queue.Push(&state{
					ore:      st.ore + dt*st.oreBots - oreOre,
					oreBots:  st.oreBots + 1,
					cly:      st.cly + dt*st.clyBots,
					clyBots:  st.clyBots,
					obs:      st.obs + dt*st.obsBots,
					obsBots:  st.obsBots,
					geo:      st.geo + dt*st.geoBots,
					geoBots:  st.geoBots,
					timeLeft: st.timeLeft - dt,
				})
				wait = false
			}
		}

		if st.clyBots < obsCly {
			dt := max((clyOre-st.ore+st.oreBots-1)/st.oreBots, 0) + 1
			if dt < st.timeLeft && dt < dtGeo {
				queue.Push(&state{
					ore:      st.ore + dt*st.oreBots - clyOre,
					oreBots:  st.oreBots,
					cly:      st.cly + dt*st.clyBots,
					clyBots:  st.clyBots + 1,
					obs:      st.obs + dt*st.obsBots,
					obsBots:  st.obsBots,
					geo:      st.geo + dt*st.geoBots,
					geoBots:  st.geoBots,
					timeLeft: st.timeLeft - dt,
				})
				wait = false
			}
		}

		if wait {
			queue.Push(&state{
				ore:      st.ore + st.timeLeft*st.oreBots,
				oreBots:  st.oreBots,
				cly:      st.cly + st.timeLeft*st.clyBots,
				clyBots:  st.clyBots,
				obs:      st.obs + st.timeLeft*st.obsBots,
				obsBots:  st.obsBots,
				geo:      st.geo + st.timeLeft*st.geoBots,
				geoBots:  st.geoBots,
				timeLeft: 0,
			})
		}
	}
}

func (aoc *aoc202219) Part1(input *Input) string {
	in := make(chan [7]int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for blueprint := range in {
				result += blueprint[0] * aoc.search(blueprint, 24)
			}
			out <- result
		}()
	}

	for blueprint := range aoc.blueprint(input) {
		in <- blueprint
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202219) Part2(input *Input) string {
	out := make(chan int)
	n := 0
	for blueprint := range aoc.blueprint(input) {
		go func() {
			out <- aoc.search(blueprint, 32)
		}()
		n++
		if n >= 3 {
			break
		}
	}

	result := 1
	for range n {
		result *= <-out
	}
	return IntResult(result)
}
