package main

import (
	"fmt"
	"math/rand"
	"os"
	"runtime"
)

func trial(r *rand.Rand) bool {
	x1 := r.Float64()
	p := 0
	for {
		x2 := r.Float64()
		if x1 > 0.5 && x1 < x2 {
			break
		} else if x1 < 0.5 && x1 > x2 {
			break
		}
		p++
		if p >= 2 {
			break
		}
		x1 = x2
	}
	return p >= 2
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range niters {
				if trial(r) {
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
	fmt.Printf("%d/%d=%f\n", count, niters*ncpu, float64(count)/float64(niters*ncpu))
}

func ectrial(r *rand.Rand) int {
	x1 := r.Float64()
	p := 0
	for {
		x2 := r.Float64()
		if x1 > 0.5 && x1 < x2 {
			break
		} else if x1 < 0.5 && x1 > x2 {
			break
		}
		p++
		x1 = x2
	}
	return p
}

type buckets struct {
	Buckets  []int
	Overflow int
}

func (b *buckets) Add(bucket buckets) {
	for i := range b.Buckets {
		b.Buckets[i] += bucket.Buckets[i]
	}
	b.Overflow += bucket.Overflow
}

func ecsimulate(niters, ncpu, nbuckets int, r *rand.Rand) {
	ch := make(chan buckets)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			b := buckets{make([]int, nbuckets), 0}
			for _ = range niters {
				p := ectrial(r)
				if p >= nbuckets {
					b.Overflow += p
				} else {
					b.Buckets[p]++
				}
			}
			ch <- b
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	b := buckets{make([]int, nbuckets), 0}
	for _ = range ncpu {
		b.Add(<-ch)
	}
	n := ncpu * niters
	sum := b.Overflow
	for i, p := range b.Buckets {
		sum += p * i
	}
	for i := range nbuckets {
		fmt.Printf("%d %d %f", i, n, float64(sum)/float64(n)-float64(i))
		lastn := n
		n -= b.Buckets[i]
		sum -= b.Buckets[i] * i
		fmt.Printf(" %f\n", float64(n)/float64(lastn))
	}
	fmt.Printf("%d %d %f\n", nbuckets, n, float64(sum)/float64(n)-float64(nbuckets))
}

func main() {
	const seed = 20250905
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	{
		const niters = 10000000
		simulate(niters, ncpu, r)
	}

	if len(os.Args) > 1 && os.Args[1] == "ec" {
		const niters = 10000000
		const nbuckets = 20
		ecsimulate(niters, ncpu, nbuckets, r)
	}
}
