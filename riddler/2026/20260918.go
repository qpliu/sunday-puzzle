package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

const (
	ABC = 0
	ACB = 1
	BAC = 2
	BCA = 3
	CAB = 4
	CBA = 5
)

var (
	swaps [6][3]int
)

func init() {
	swaps[ABC] = [3]int{BAC, CBA, ACB}
	swaps[ACB] = [3]int{CAB, BCA, ABC}
	swaps[BAC] = [3]int{ABC, CAB, BCA}
	swaps[BCA] = [3]int{CBA, ACB, BAC}
	swaps[CAB] = [3]int{ACB, BAC, CBA}
	swaps[CBA] = [3]int{BCA, ABC, CAB}
}

func trial(r *rand.Rand) (int, int) {
	state := ABC
	visited := 1 << ABC
	count := 0
	returned := 0
	all6 := 0
	for returned == 0 || all6 == 0 {
		count++
		state = swaps[state][r.Intn(3)]
		if returned == 0 && state == ABC {
			returned = count
		}
		visited |= 1 << state
		if all6 == 0 && visited == 0x3f {
			all6 = count
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
