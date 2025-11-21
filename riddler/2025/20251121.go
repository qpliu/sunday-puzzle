package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(students []int, r *rand.Rand) bool {
	for {
		r.Shuffle(len(students), func(i, j int) {
			students[i], students[j] = students[j], students[i]
		})
		drewSelf := false
		for i, s := range students {
			if i == s {
				drewSelf = true
			}
		}
		if !drewSelf {
			break
		}
	}
	n := 0
	i := 0
	for {
		n++
		i = students[i]
		if i == 0 {
			return n == len(students)
		}
	}
}

func simulate(niters, ncpu, nStudents int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			students := make([]int, nStudents)
			for i := range students {
				students[i] = i
			}
			for _ = range niters {
				if trial(students, r) {
					count++
				}
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}
	fmt.Printf("%d: %f %f\n", nStudents, float64(count)/float64(niters*ncpu), math.E/float64(nStudents))
}

func main() {
	const seed = 20251121
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 2000000
	for _, nStudents := range []int{5, 10, 25, 50, 100, 150} {
		simulate(niters, ncpu, nStudents, r)
	}
}
