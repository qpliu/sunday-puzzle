package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

func trial(test bool, ngrid int, r *rand.Rand) float64 {
	var x1, y1, x2, y2 float64
	for {
		x1 = 2*r.Float64() - 1
		y1 = 2*r.Float64() - 1
		if x1*x1+y1*y1 <= 1.0 {
			break
		}
	}
	for {
		x2 = 2*r.Float64() - 1
		y2 = 2*r.Float64() - 1
		if x2*x2+y2*y2 <= 1.0 {
			break
		}
	}
	n1, n2 := 0, 0
	count := func(x, y float64) {
		if (x-x1)*(x-x1)+(y-y1)*(y-y1) > (x-x2)*(x-x2)+(y-y2)*(y-y2) {
			n2++
		} else {
			n1++
		}
	}
	for i := range ngrid {
		x := float64(i+1) / float64(ngrid)
		count(x, 0)
		count(-x, 0)
		count(0, x)
		count(0, -x)
		for j := range ngrid {
			y := float64(j+1) / float64(ngrid)
			if x*x+y*y > 1.0 {
				continue
			}
			count(x, y)
			count(-x, y)
			count(x, -y)
			count(-x, -y)
		}
	}

	nf := float64(1+max(n1, n2)) / float64(n1+n2)

	if ngrid == 0 || test {
		r1 := math.Sqrt(x1*x1 + y1*y1)
		r2 := math.Sqrt(x2*x2 + y2*y2)
		t1 := math.Atan2(y1, x1)
		t2 := math.Atan2(y2, x2)
		rd := math.Sqrt((r1*r1*r1*r1-2*r1*r1*r2*r2+r2*r2*r2*r2)/(r1*r1+r2*r2-2*r1*r2*math.Cos(t1-t2))) / 2
		gf := 0.5 + (math.Sqrt(1-rd*rd)*rd+math.Asin(rd))/math.Pi
		if !test {
			return gf
		}
		fmt.Printf("r1=%f r2=%f t1=%f t2=%f t=%f rd=%f gf=%f nf=%f diff=%f\n", r1, r2, t1, t2, math.Abs(t1-t2)/math.Pi, rd, gf, nf, math.Abs(nf-gf))
	}

	return nf
}

func simulate(niter, ngrid int, r *rand.Rand) {
	ch := make(chan float64)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			sum := float64(0)
			for range niter {
				sum += trial(false, ngrid, r)
			}
			ch <- sum / float64(niter)
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sum := float64(0)
	for range runtime.NumCPU() {
		sum += <-ch
	}
	fmt.Printf("%d,%d: %f\n", niter, ngrid, sum/float64(runtime.NumCPU()))
}

func main() {
	const seed = 20260306
	r := rand.New(rand.NewSource(seed))
	{
		const ngrid = 0
		for _, niter := range [...]int{1000, 10000, 100000, 1000000} {
			simulate(niter, ngrid, r)
		}
	}
	{
		const ngrid = 100
		for _, niter := range [...]int{1000, 10000, 100000} {
			simulate(niter, ngrid, r)
		}
	}
	{
		const ngrid = 1000
		for _, niter := range [...]int{1000} {
			simulate(niter, ngrid, r)
		}
	}
}
