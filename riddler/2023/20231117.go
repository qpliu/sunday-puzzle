package main

import (
	"fmt"
	"math/rand"
	"sort"
)

func trial(r *rand.Rand) [4]int {
	result := [4]int{0,0,0,0}
	for i := 0; i < 8; i++ {
		switch r.Intn(8) {
		case 0:
			result[0]++
		case 1, 2, 3:
			result[1]++
		case 4, 5, 6:
			result[2]++
		case 7:
			result[3]++
		default:
			panic("")
		}
	}
	return result
}

func simulate(n int, r *rand.Rand) (int, map[[4]int]int) {
	count := 0
	histogram := make(map[[4]int]int)
	for i := 0; i < n; i++ {
		t := trial(r)
		if t == [4]int{1,3,3,1} {
			count++
		}
		sort.Ints(t[:])
		histogram[t]++
	}
	return count, histogram
}

func main() {
	const n = 10000000
	const seed = 718355
	count, histogram := simulate(n, rand.New(rand.NewSource(seed)))
	fmt.Printf("%d %f\n", count, float64(count)/n)
	for q, occurrences := range histogram {
		fmt.Printf("%d: %d %f\n", q, occurrences, float64(occurrences)/n)
	}
}
