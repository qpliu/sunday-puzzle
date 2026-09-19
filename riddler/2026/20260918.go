package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) (int, int) {
	state := [3]byte{'A', 'B', 'C'}
	visited := map[[3]byte]bool{}
	visited[state] = true
	count := 0
	returned := 0
	all6 := 0
	for returned == 0 || all6 == 0 {
		count++
		i := r.Intn(3)
		j := r.Intn(2)
		if j >= i {
			j++
		}
		state[i], state[j] = state[j], state[i]
		if returned == 0 && state == [3]byte{'A', 'B', 'C'} {
			returned = count
		}
		if all6 == 0 {
			visited[state] = true
			if len(visited) == 6 {
				all6 = count
			}
		}
	}
	return returned, all6
}

func simulate(niter int, r *rand.Rand) {
	ch := make(chan [2]float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			counts := [2]int{}
			for range niter {
				returned, all6 := trial(r)
				counts[0] += returned
				counts[1] += all6
			}
			avgs := [2]float64{}
			for i, count := range counts {
				avgs[i] = float64(count) / float64(niter)
			}
			ch <- avgs
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := [2]float64{}
	for range runtime.NumCPU() {
		avgs := <-ch
		for i, avg := range avgs {
			sums[i] += avg
		}
	}
	for _, sum := range sums {
		fmt.Printf("%f\n", sum/float64(runtime.NumCPU()))
	}
}

func main() {
	const seed = 20260918
	r := rand.New(rand.NewSource(seed))

	const niter = 10000000
	simulate(niter, r)
}
