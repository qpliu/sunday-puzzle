package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(items []int, r *rand.Rand) int {
	r.Shuffle(len(items), func(i, j int) {
		items[i], items[j] = items[j], items[i]
	})
	seqs := make([]int, len(items))
	for i := len(items) - 1; i >= 0; i-- {
		seqLen := 1
		for j := items[i] + 1; j < len(items); j++ {
			seqLen = max(seqLen, seqs[j]+1)
		}
		seqs[items[i]] = seqLen
	}
	maxLen := 0
	for _, seq := range seqs {
		maxLen = max(maxLen, seq)
	}
	return maxLen
}

func simulate(n, niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			items := make([]int, n)
			for i := range items {
				items[i] = i
			}
			for _ = range niters {
				count += trial(items, r)
			}
			ch <- count
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	count := 0
	for _ = range ncpu {
		count += <-ch
	}
	fmt.Printf("%d/%d=%f\n", count, niters*ncpu, float64(count)/float64(niters*ncpu))
}

func main() {
	const seed = 20250919
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	{
		const niters = 1000000
		const n = 4
		const nec = 10
		simulate(n, niters, ncpu, r)
		simulate(nec, niters, ncpu, r)
	}
}
