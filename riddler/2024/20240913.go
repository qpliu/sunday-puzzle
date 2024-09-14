package main

import (
	"fmt"
	"math"
	"math/rand"
	"os"
	"runtime"
)

var (
	p6  [64]float64
	p12 [512]float64
)

func init() {
	for tiles := range p6 {
		if tiles == 0 {
			p6[tiles] = 1
		} else {
			for i := range 6 {
				roll := i + 1
				set := map[int]bool{}
				remainingTiles(tiles, roll, set)
				p := float64(0)
				for j := range set {
					p = max(p, p6[j])
				}
				p6[tiles] += p / 6
			}
		}
	}
	for tiles := range p12 {
		if tiles == 0 {
			p12[tiles] = 1
		} else {
			for i := range 11 {
				roll := i + 2
				set := map[int]bool{}
				remainingTiles(tiles, roll, set)
				p := float64(0)
				for j := range set {
					p = max(p, p12[j])
				}
				p12[tiles] += p * (6 - math.Abs(float64(roll-7))) / 36
			}
		}
	}
}

func remainingTiles(tiles, sum int, remainingTilesSet map[int]bool) {
	for tile := 1; tile <= sum; tile++ {
		x := 1 << (tile - 1)
		if tiles&x != 0 {
			remaining := tiles ^ x
			if tile == sum {
				remainingTilesSet[remaining] = true
			} else {
				remainingTiles(remaining, sum-tile, remainingTilesSet)
			}
		}
	}
}

func trial(r *rand.Rand) bool {
	tiles := 0x3f
	for {
		set := map[int]bool{}
		remainingTiles(tiles, r.Intn(6)+1, set)
		hasOption := false
		option := 0
		optionScore := float64(0)
		for i := range set {
			if !hasOption || optionScore < p6[i] {
				option = i
				optionScore = p6[i]
			}
			hasOption = true
		}
		if !hasOption {
			return false
		}
		if option == 0 {
			return true
		}
		tiles = option
	}
}

func simulate(n, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range n {
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
	fmt.Printf("f: %f\n", float64(count)/float64(n*ncpu))
}

func ectrial(r *rand.Rand) bool {
	tiles := 0x1ff
	for {
		set := map[int]bool{}
		remainingTiles(tiles, r.Intn(6)+r.Intn(6)+2, set)
		hasOption := false
		option := 0
		optionScore := float64(0)
		for i := range set {
			if !hasOption || optionScore < p12[i] {
				option = i
				optionScore = p12[i]
			}
			hasOption = true
		}
		if !hasOption {
			return false
		}
		if option == 0 {
			return true
		}
		tiles = option
	}
}

func ecsimulate(n, ncpu int, r *rand.Rand) {
	ch := make(chan int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			count := 0
			for _ = range n {
				if ectrial(r) {
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
	fmt.Printf("ec: %f\n", float64(count)/float64(n*ncpu))
}

func main() {
	const seed = 20240913
	r := rand.New(rand.NewSource(seed))
	const n = 1000000
	ncpu := runtime.NumCPU()

	if len(os.Args) == 2 && os.Args[1] == "check" {
		fmt.Printf("%f\n", p6[0x3f])
		for _, tiles := range []int{0x3e, 0x3d, 0x3b, 0x37, 0x2f, 0x1f} {
			fmt.Printf(" %f", p6[tiles])
		}
		fmt.Printf("\n")
		fmt.Printf("%f\n", p12[0x1ff])
		for _, tiles := range []int{0x1fe, 0x1fd, 0x1fb, 0x1f7, 0x1ef, 0x1df, 0x1bf, 0x17f, 0x0ff} {
			fmt.Printf(" %f", p12[tiles])
		}
		fmt.Printf("\n")
	}

	if len(os.Args) < 2 || os.Args[1] == "f" {
		simulate(n, ncpu, r)
	}

	if len(os.Args) < 2 || os.Args[1] == "ec" {
		ecsimulate(n, ncpu, r)
	}
}
