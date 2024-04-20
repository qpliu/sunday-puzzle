package main

import (
	"fmt"
	"math/rand"
)

func simulate(n int, r *rand.Rand) int {
	nclusters := 0
	for i := 0; i < n; i++ {
		if r.Intn(2) == 0 {
			nclusters++
		}
	}
	return nclusters
}

func ecsimulate(n int, r *rand.Rand) int {
	nclusters := 0
	bothSame := r.Intn(2) == 0
	for i := 0; i < n; i++ {
		if bothSame {
			switch r.Intn(4) {
			case 0:
				// extends cluster
			case 1:
				// extends cluster, starts new cluster of opposite cluster
				bothSame = false
			case 2:
				// extends cluster, starts new cluster of opposite cluster
				bothSame = false
			case 3:
				// starts new cluster of opposite color
				nclusters++
			}
		} else {
			switch r.Intn(4) {
			case 0:
				// extends one cluster, ends one cluster
				nclusters++
				bothSame = true
			case 1:
				// extends both clusters
			case 2:
				// ends both clusters
				nclusters += 2
			case 3:
				// extends one cluster, ends one cluster
				nclusters++
				bothSame = true
			}
		}
	}
	return nclusters
}

func fsimulate(n int, r *rand.Rand, index int) int {
	sumSizes := 0
	for i := 0; i < n; i++ {
		size := 0
		for j := 0; j < 2*index; j++ {
			size++
			if r.Intn(2) == 0 {
				if j < index {
					size = 0
				} else {
					break
				}
			}
		}
		sumSizes += size
	}
	return sumSizes
}

func main() {
	const n = 50000000
	const seed = 3851367828
	r := rand.New(rand.NewSource(seed))
	nclusters := simulate(n, r)
	fmt.Printf("%d/%d %f\n", n, nclusters, n/float64(nclusters))
	nclusters = ecsimulate(n, r)
	fmt.Printf("%d/%d %f\n", 2*n, nclusters, 2*n/float64(nclusters))

	const index = 50
	sumSizes := fsimulate(n, r, index)
	fmt.Printf("%d/%d %f\n", sumSizes, n, float64(sumSizes)/n)
}
