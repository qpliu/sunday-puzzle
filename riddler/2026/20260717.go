package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(s float64, r *rand.Rand) bool {
	if s < r.Float64() {
		return false
	}
	s1 := max(r.Float64(), r.Float64())
	return s < s1
}

func simulate(niter, nbucket int, r *rand.Rand) {
	sep := "line([\n"
	maxP := float64(0)
	maxS := float64(0)
	for is := range nbucket {
		s := float64(is) / float64(nbucket)

		ch := make(chan float64)
		for range runtime.NumCPU() {
			go func(s float64, r *rand.Rand) {
				count := 0
				for range niter {
					if trial(s, r) {
						count++
					}
				}
				ch <- float64(count) / float64(niter)
			}(s, rand.New(rand.NewSource(int64(r.Uint64()))))
		}
		p := float64(0)
		for range runtime.NumCPU() {
			p += <-ch
		}
		p /= float64(runtime.NumCPU())
		fmt.Printf("%s(%f,%f)", sep, s, p)
		sep = ","
		if p > maxP {
			maxS, maxP = s, p
		}
	}
	fmt.Printf("\n])\n%f,%f\n", maxS, maxP)
}

func ectrial(q, s float64, r *rand.Rand) bool {
	if q < r.Float64() {
		return false
	}
	s1 := 1 - max(r.Float64(), r.Float64())
	if s < r.Float64()*s1 {
		return false
	}
	e2 := 1 - max(r.Float64(), r.Float64())
	e3 := 1 - max(r.Float64(), r.Float64())
	s2 := e2 * r.Float64()
	s3 := e3 * r.Float64()
	if s2 > s3 {
		return 1-q-s > e2-s2
	} else {
		return 1-q-s > e3-s3
	}
}

func ecsimulate(niter, nbucket int, r *rand.Rand) {
	sepQ := "    data = [["
	maxP := float64(0)
	maxQ := float64(0)
	maxS := float64(0)
	for iq := range nbucket {
		q := float64(iq) / float64(nbucket)
		sep := sepQ
		sepQ = "],\n["
		for is := range nbucket {
			s := float64(is) / float64(nbucket)
			if q+s >= 1.0 {
				fmt.Printf("%s0", sep)
				sep = ","
				continue
			}

			ch := make(chan float64)
			for range runtime.NumCPU() {
				go func(q, s float64, r *rand.Rand) {
					count := 0
					for range niter {
						if ectrial(q, s, r) {
							count++
						}
					}
					ch <- float64(count) / float64(niter)
				}(q, s, rand.New(rand.NewSource(int64(r.Uint64()))))
			}
			p := float64(0)
			for range runtime.NumCPU() {
				p += <-ch
			}
			p /= float64(runtime.NumCPU())
			fmt.Printf("%s%f", sep, p)
			sep = ","
			if p > maxP {
				maxQ, maxS, maxP = q, s, p
			}
		}
	}
	fmt.Printf("]]\n%f,%f,%f\n", maxQ, maxS, maxP)
}

func main() {
	const seed = 20260717
	r := rand.New(rand.NewSource(seed))

	{
		const niter = 1000000
		const nbucket = 50
		simulate(niter, nbucket, r)
	}
	{
		const niter = 100000
		const nbucket = 40
		ecsimulate(niter, nbucket, r)
	}
}
