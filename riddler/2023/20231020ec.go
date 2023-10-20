package main

import (
	"math/rand"
)

func trial(r *rand.Rand) int {
	lead := 1
	t := 1
	for {
		switch r.Intn(4) {
		case 0, 1:
			t++
		case 2:
			if lead <= 1 {
				return t
			}
			t++
			lead--
		case 3:
			t++
			lead++
		default:
			panic("")
		}
	}
}

func simulate(n int, r *rand.Rand) float64 {
	var total float64
	for i := 0; i < n; i++ {
		t := trial(r)
		total += float64(t) / float64(n)
		if i < 10 || n-i < 10 {
			println(i, t)
		}
	}
	return total
}

func main() {
	const n = 5000
	const seed = 306387993
	println(simulate(n, rand.New(rand.NewSource(seed))))
}
