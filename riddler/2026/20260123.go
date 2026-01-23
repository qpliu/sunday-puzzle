package main

import (
	"fmt"
	"math/bits"
	"math/rand"
	"runtime"
)

var bingo3 = [...]int{
	(1 << 0) | (1 << 1) | (1 << 2),
	(1 << 3) | /********/ (1 << 4),
	(1 << 5) | (1 << 6) | (1 << 7),
	(1 << 0) | (1 << 3) | (1 << 5),
	(1 << 1) | /********/ (1 << 6),
	(1 << 2) | (1 << 4) | (1 << 7),
	(1 << 0) | /********/ (1 << 7),
	(1 << 2) | /********/ (1 << 5),
}

func trial3(numbers []int, r *rand.Rand) int {
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	grid := 0
	for i, n := range numbers {
		grid |= n
		for _, bingo := range bingo3 {
			if grid&bingo == bingo {
				return i + 1
			}
		}
	}
	panic("?")
}

var bingo5 = [...]int{
	(1 << 0) | (1 << 1) | (1 << 2) | (1 << 3) | (1 << 4),
	(1 << 5) | (1 << 6) | (1 << 7) | (1 << 8) | (1 << 9),
	(1 << 10) | (1 << 11) | /*********/ (1 << 12) | (1 << 13),
	(1 << 14) | (1 << 15) | (1 << 16) | (1 << 17) | (1 << 18),
	(1 << 19) | (1 << 20) | (1 << 21) | (1 << 22) | (1 << 23),
	(1 << 0) | (1 << 5) | (1 << 10) | (1 << 14) | (1 << 19),
	(1 << 1) | (1 << 6) | (1 << 11) | (1 << 15) | (1 << 20),
	(1 << 2) | (1 << 7) | /*********/ (1 << 16) | (1 << 21),
	(1 << 3) | (1 << 8) | (1 << 12) | (1 << 17) | (1 << 22),
	(1 << 4) | (1 << 9) | (1 << 13) | (1 << 18) | (1 << 23),
	(1 << 0) | (1 << 6) | /*********/ (1 << 17) | (1 << 23),
	(1 << 4) | (1 << 8) | /*********/ (1 << 15) | (1 << 19),
}

func trial5(numbers []int, r *rand.Rand) int {
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	grid := 0
	for i, n := range numbers {
		grid |= n
		for _, bingo := range bingo5 {
			if grid&bingo == bingo {
				return i + 1
			}
		}
	}
	panic("?")
}

func simulate3(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			numbers := make([]int, 8)
			for i := range numbers {
				numbers[i] = 1 << i
			}
			sum := 0
			for range niter {
				sum += trial3(numbers, r)
			}
			ch <- float64(sum) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("3-by-3, %d trials: %f markers\n", runtime.NumCPU()*niter, sum/float64(runtime.NumCPU()))
}

func simulate5(niter int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			numbers := make([]int, 24)
			for i := range numbers {
				numbers[i] = 1 << i
			}
			sum := 0
			for range niter {
				sum += trial5(numbers, r)
			}
			ch <- float64(sum) / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("5-by-5, %d trials: %f markers\n", runtime.NumCPU()*niter, sum/float64(runtime.NumCPU()))
}

func fttrial3(numbers []int, r *rand.Rand) (int, int) {
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	marked := 0
	grid := [76]int{}
	for _, n := range numbers {
		if n >= 16 && n <= 30 {
			if marked&(1<<0) == 0 {
				grid[n] = 1 << 0
				marked |= 1 << 0
			} else if marked&(1<<3) == 0 {
				grid[n] = 1 << 3
				marked |= 1 << 3
			} else if marked&(1<<5) == 0 {
				grid[n] = 1 << 5
				marked |= 1 << 5
			}
		} else if n >= 31 && n <= 45 {
			if marked&(1<<1) == 0 {
				grid[n] = 1 << 1
				marked |= 1 << 1
			} else if marked&(1<<6) == 0 {
				grid[n] = 1 << 6
				marked |= 1 << 6
			}
		} else if n >= 46 && n <= 60 {
			if marked&(1<<2) == 0 {
				grid[n] = 1 << 2
				marked |= 1 << 2
			} else if marked&(1<<4) == 0 {
				grid[n] = 1 << 4
				marked |= 1 << 4
			} else if marked&(1<<7) == 0 {
				grid[n] = 1 << 7
				marked |= 1 << 7
			}
		}
	}
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	marked = 0
	for i, n := range numbers {
		marked |= grid[n]
		for _, bingo := range bingo3 {
			if marked&bingo == bingo {
				return i + 1, bits.OnesCount(uint(marked))
			}
		}
	}
	panic("?")
}

func fttrial5(numbers []int, r *rand.Rand) (int, int) {
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	marked := 0
	grid := [76]int{}
	for _, n := range numbers {
		if n >= 1 && n <= 15 {
			if marked&(1<<0) == 0 {
				grid[n] = 1 << 0
				marked |= 1 << 0
			} else if marked&(1<<5) == 0 {
				grid[n] = 1 << 5
				marked |= 1 << 5
			} else if marked&(1<<10) == 0 {
				grid[n] = 1 << 10
				marked |= 1 << 10
			} else if marked&(1<<14) == 0 {
				grid[n] = 1 << 14
				marked |= 1 << 14
			} else if marked&(1<<19) == 0 {
				grid[n] = 1 << 19
				marked |= 1 << 19
			}
		} else if n >= 16 && n <= 30 {
			if marked&(1<<1) == 0 {
				grid[n] = 1 << 1
				marked |= 1 << 1
			} else if marked&(1<<6) == 0 {
				grid[n] = 1 << 6
				marked |= 1 << 6
			} else if marked&(1<<11) == 0 {
				grid[n] = 1 << 11
				marked |= 1 << 11
			} else if marked&(1<<15) == 0 {
				grid[n] = 1 << 15
				marked |= 1 << 15
			} else if marked&(1<<20) == 0 {
				grid[n] = 1 << 20
				marked |= 1 << 20
			}
		} else if n >= 31 && n <= 45 {
			if marked&(1<<2) == 0 {
				grid[n] = 1 << 2
				marked |= 1 << 2
			} else if marked&(1<<7) == 0 {
				grid[n] = 1 << 7
				marked |= 1 << 7
			} else if marked&(1<<16) == 0 {
				grid[n] = 1 << 16
				marked |= 1 << 16
			} else if marked&(1<<21) == 0 {
				grid[n] = 1 << 21
				marked |= 1 << 21
			}
		} else if n >= 46 && n <= 60 {
			if marked&(1<<3) == 0 {
				grid[n] = 1 << 3
				marked |= 1 << 3
			} else if marked&(1<<8) == 0 {
				grid[n] = 1 << 8
				marked |= 1 << 8
			} else if marked&(1<<12) == 0 {
				grid[n] = 1 << 12
				marked |= 1 << 12
			} else if marked&(1<<17) == 0 {
				grid[n] = 1 << 17
				marked |= 1 << 17
			} else if marked&(1<<22) == 0 {
				grid[n] = 1 << 22
				marked |= 1 << 22
			}
		} else if n >= 61 && n <= 75 {
			if marked&(1<<4) == 0 {
				grid[n] = 1 << 4
				marked |= 1 << 4
			} else if marked&(1<<9) == 0 {
				grid[n] = 1 << 9
				marked |= 1 << 9
			} else if marked&(1<<13) == 0 {
				grid[n] = 1 << 13
				marked |= 1 << 13
			} else if marked&(1<<18) == 0 {
				grid[n] = 1 << 18
				marked |= 1 << 18
			} else if marked&(1<<23) == 0 {
				grid[n] = 1 << 23
				marked |= 1 << 23
			}
		}
	}
	r.Shuffle(len(numbers), func(i, j int) {
		numbers[i], numbers[j] = numbers[j], numbers[i]
	})
	marked = 0
	for i, n := range numbers {
		marked |= grid[n]
		for _, bingo := range bingo5 {
			if marked&bingo == bingo {
				return i + 1, bits.OnesCount(uint(marked))
			}
		}
	}
	panic("?")
}

func ftsimulate3(niter int, r *rand.Rand) {
	ch := make(chan [2]float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			numbers := make([]int, 45)
			for i := range numbers {
				numbers[i] = 16 + i
			}
			sumCalls, sumMarkers := 0, 0
			for range niter {
				calls, markers := fttrial3(numbers, r)
				sumCalls += calls
				sumMarkers += markers
			}
			ch <- [2]float64{float64(sumCalls) / float64(niter), float64(sumMarkers) / float64(niter)}
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sumCalls := float64(0)
	sumMarkers := float64(0)
	for range runtime.NumCPU() {
		sums := <-ch
		sumCalls += sums[0]
		sumMarkers += sums[1]
	}
	fmt.Printf("3-by-3, %d trials: %f calls, %f markers\n", runtime.NumCPU()*niter, sumCalls/float64(runtime.NumCPU()), sumMarkers/float64(runtime.NumCPU()))
}

func ftsimulate5(niter int, r *rand.Rand) {
	ch := make(chan [2]float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			numbers := make([]int, 75)
			for i := range numbers {
				numbers[i] = 1 + i
			}
			sumCalls, sumMarkers := 0, 0
			for range niter {
				calls, markers := fttrial5(numbers, r)
				sumCalls += calls
				sumMarkers += markers
			}
			ch <- [2]float64{float64(sumCalls) / float64(niter), float64(sumMarkers) / float64(niter)}
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sumCalls := float64(0)
	sumMarkers := float64(0)
	for range runtime.NumCPU() {
		sums := <-ch
		sumCalls += sums[0]
		sumMarkers += sums[1]
	}
	fmt.Printf("5-by-5, %d trials: %f calls, %f markers\n", runtime.NumCPU()*niter, sumCalls/float64(runtime.NumCPU()), sumMarkers/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260123
	r := rand.New(rand.NewSource(seed))

	for _, niter := range [...]int{100000, 1000000, 10000000} {
		simulate3(niter, r)
		simulate5(niter, r)
		ftsimulate3(niter, r)
		ftsimulate5(niter, r)
	}
}
