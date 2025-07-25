package main

import (
	"fmt"
	"math/rand"
	"runtime"
)

func trial(r *rand.Rand) (bool, int) {
	x := r.Float64()
	y := r.Float64()
	x, y = min(x, y), max(x, y)
	z := r.Float64()
	s := r.Float64()

	win := false
	switch {
	case z > s:
		win = z > y || y <= 0.5
	case y > 0.5:
		win = false
	default:
		win = r.Intn(3) == 0
	}

	ordering := 0
	switch {
	case x <= y && y <= z && z <= s:
		ordering = 0
	case x <= y && y <= s && s <= z:
		ordering = 1
	case x <= z && z <= y && y <= s:
		ordering = 2
	case x <= z && z <= s && s <= y:
		ordering = 3
	case x <= s && s <= y && y <= z:
		ordering = 4
	case x <= s && s <= z && z <= y:
		ordering = 5
	case z <= x && x <= y && y <= s:
		ordering = 6
	case z <= x && x <= s && s <= y:
		ordering = 7
	case z <= s && s <= x && x <= y:
		ordering = 8
	case s <= x && x <= y && y <= z:
		ordering = 9
	case s <= x && x <= z && s <= y:
		ordering = 10
	case s <= z && z <= x && x <= y:
		ordering = 11
	default:
		panic(fmt.Sprintf("x=%f y=%f z=%f s=%f", x, y, z, s))
	}

	return win, ordering
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan [13]int)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			var counts [13]int
			for _ = range niters {
				win, ordering := trial(r)
				if win {
					counts[12]++
					counts[ordering]++
				}
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	var counts [13]int
	for _ = range ncpu {
		c := <-ch
		for i := range c {
			counts[i] += c[i]
		}
	}
	denom := float64(ncpu * niters)
	fmt.Printf("%d/%d %f\n", counts[12], ncpu*niters, float64(counts[12])/denom)
	fmt.Printf("pxyzs=%f\n", float64(counts[0])/denom)
	fmt.Printf("pxysz=%f\n", float64(counts[1])/denom)
	fmt.Printf("pxzys=%f\n", float64(counts[2])/denom)
	fmt.Printf("pxzsy=%f\n", float64(counts[3])/denom)
	fmt.Printf("pxsyz=%f\n", float64(counts[4])/denom)
	fmt.Printf("pxszy=%f\n", float64(counts[5])/denom)
	fmt.Printf("pzxys=%f\n", float64(counts[6])/denom)
	fmt.Printf("pzxsy=%f\n", float64(counts[7])/denom)
	fmt.Printf("pzsxy=%f\n", float64(counts[8])/denom)
	fmt.Printf("psxyz=%f\n", float64(counts[9])/denom)
	fmt.Printf("psxzy=%f\n", float64(counts[10])/denom)
	fmt.Printf("pszxy=%f\n", float64(counts[11])/denom)
}

func main() {
	const seed = 20250725
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 1000000
	simulate(niters, ncpu, r)
}
