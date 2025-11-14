package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func dist(x1, y1, x2, y2 float64) float64 {
	return math.Sqrt((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2))
}

func trial(r *rand.Rand) [3]float64 {
	xt := float64(1)
	yt := float64(1)
	for {
		xt = r.Float64()
		yt = r.Float64()
		if xt*xt+yt*yt < 1 {
			break
		}
	}
	theta := r.Float64() * math.Pi
	xh := math.Cos(theta)
	yh := math.Sin(theta)
	if xh*xh+yt*yt > 1 {
		return [3]float64{
			math.Abs(xh),
			0,
			min(dist(xt, yt, xh, yh), dist(xt, yt, xh, -yh)),
		}
	} else {
		return [3]float64{
			math.Abs(xh),
			math.Abs(xt - xh),
			0,
		}
	}
}

func simulate(niters, ncpu int, r *rand.Rand) {
	ch := make(chan [3]float64)
	for _ = range ncpu {
		go func(r *rand.Rand) {
			sums := [3]float64{0, 0, 0}
			for _ = range niters {
				for i, d := range trial(r) {
					sums[i] += d
				}
			}
			ch <- sums
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := [3]float64{0, 0, 0}
	for _ = range ncpu {
		for i, d := range <-ch {
			sums[i] += d
		}
	}
	fmt.Printf("f:%f %f\n", sums[0]/float64(niters*ncpu), 2/math.Pi)
	fmt.Printf("ec:%f\n", (sums[1]+sums[2])/float64(niters*ncpu))
	fmt.Printf("ec1:%f %f\n", sums[1]/float64(niters*ncpu), 40/(9*math.Pi*math.Pi))
	fmt.Printf("ec2:%f\n", sums[2]/float64(niters*ncpu))
}

func main() {
	const seed = 20251114
	r := rand.New(rand.NewSource(seed))
	ncpu := runtime.NumCPU()

	const niters = 10000000
	simulate(niters, ncpu, r)
}
