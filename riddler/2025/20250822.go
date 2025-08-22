package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) int64 {
	d := int64(0)
	for {
		loop := int64(0)
		switch r.Intn(4) {
		case 0:
			loop = 10
		case 1:
			loop = 30
		case 2:
			loop = 35
		case 3:
			loop = 45
		default:
			panic("?")
		}
		if loop+d > 65 {
			return d
		}
		d += loop
	}
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sum := int64(0)
			for _ = range niters {
				sum += trial(r)
			}
			ch <- sum
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := int64(0)
	for _ = range ncpu {
		sum += <-ch
	}
	fmt.Printf("%f\n", float64(sum)/float64(10*ncpu*niters))
}

func ecTrial(r *rand.Rand) int64 {
	d := int64(0)
	mulligan := false
	for {
		loop := int64(0)
		switch r.Intn(4) {
		case 0:
			loop = 10
			if !mulligan && (d == 20 || d == 30) {
				mulligan = true
				continue
			}
		case 1:
			loop = 30
			if !mulligan && (d == 10 || d == 20) {
				mulligan = true
				continue
			}
		case 2:
			loop = 35
			if !mulligan && d == 10 {
				mulligan = true
				continue
			}
		case 3:
			loop = 45
		default:
			panic("?")
		}
		if loop+d > 65 {
			if mulligan {
				return d
			} else {
				mulligan = true
				continue
			}
		}
		d += loop
	}
}

func ecSimulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan int64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sum := int64(0)
			for _ = range niters {
				sum += ecTrial(r)
			}
			ch <- sum
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := int64(0)
	for _ = range ncpu {
		sum += <-ch
	}
	fmt.Printf("%f\n", float64(sum)/float64(10*ncpu*niters))
}

func main() {
	const seed = 20250822
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 10000000
	simulate(niters, ncpu, r)
	ecSimulate(niters, ncpu, r)
}
