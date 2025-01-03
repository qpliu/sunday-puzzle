package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

type contestant = [2]int

func round(graph [][]contestant, r *rand.Rand) [][]contestant {
	neighbors := graph[r.Intn(len(graph))]
	winner := neighbors[0]
	loser := neighbors[1+r.Intn(len(neighbors)-1)]
	if r.Intn(2) == 0 {
		winner, loser = loser, winner
	}
	result := make([][]contestant, 0, len(graph)-1)
	for _, t := range graph {
		if t[0] == winner {
			newT := []contestant{t[0]}
			for _, n := range t[1:] {
				if n != loser {
					newT = append(newT, n)
				}
			}
			for _, t2 := range graph {
				if t2[0] == loser {
					for _, n := range t2[1:] {
						add := true
						for _, n2 := range newT {
							if n2 == n {
								add = false
								break
							}
						}
						if add {
							newT = append(newT, n)
						}
					}
					break
				}
			}
			result = append(result, newT)
		} else if t[0] == loser {
			continue
		} else {
			newT := []contestant{t[0]}
			needWinner := false
			for _, n := range t[1:] {
				if n == winner || n == loser {
					needWinner = true
				} else {
					newT = append(newT, n)
				}
			}
			if needWinner {
				newT = append(newT, winner)
			}
			result = append(result, newT)
		}
	}
	return result
}

func game(xsize, ysize int, r *rand.Rand) contestant {
	graph := make([][]contestant, 0, xsize*ysize)
	for i := range xsize {
		for j := range ysize {
			neighbors := []contestant{contestant{i, j}}
			if i > 0 {
				neighbors = append(neighbors, contestant{i - 1, j})
			}
			if j > 0 {
				neighbors = append(neighbors, contestant{i, j - 1})
			}
			if j < ysize-1 {
				neighbors = append(neighbors, contestant{i, j + 1})
			}
			if i < xsize-1 {
				neighbors = append(neighbors, contestant{i + 1, j})
			}
			graph = append(graph, neighbors)
		}
	}
	for len(graph) > 1 {
		graph = round(graph, r)
	}
	return graph[0][0]
}

func simulate(n, ncpu, xsize, ysize int, r *rand.Rand) {
	ch := make(chan map[contestant]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			hist := make(map[contestant]int)
			for _ = range n {
				w := game(xsize, ysize, r)
				hist[w] = hist[w] + 1
			}
			ch <- hist
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	hist := make(map[contestant]int)
	for _ = range ncpu {
		for k, v := range <-ch {
			hist[k] = hist[k] + v
		}
	}
	fmt.Printf("%d×%d:\n", xsize, ysize)
	for i := range xsize {
		for j := range ysize {
			c := hist[contestant{i, j}]
			fmt.Printf("%d,%d: %f\n", i, j, float64(c)/float64(n*ncpu))
		}
	}
}

func main() {
	const seed = 20241213
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	{
		const n = 100000
		const xsize = 3
		const ysize = 3
		simulate(n, ncpu, xsize, ysize, r)
	}
	{
		const n = 100000
		const xsize = 4
		const ysize = 3
		simulate(n, ncpu, xsize, ysize, r)
	}
	{
		const n = 100000
		const xsize = 4
		const ysize = 4
		simulate(n, ncpu, xsize, ysize, r)
	}
}
