package main

import (
	"fmt"
	"math"
	"math/rand"
	"runtime"
)

const (
	tgreen = iota
	tyellow
	tgray1
	tgray2
	lblue
	lyellow
	lred
	lgray
	ddblue
	ddred
	ddyellow
	ddgray
	dugray
	duyellow
	dured
	dublue
	rgray
	rred
	ryellow
	rblue
	bgray
	byellow
	bgreen
	bblue
	_last_connector
)

func make_diagrams(r *rand.Rand) map[[5]int]bool {
	set := make(map[[5]int]bool)

	for nw := range 8 {
		for ne := range 8 {
			if nw >= ne {
				set[[5]int{nw, ne, r.Intn(3), r.Intn(3), tgreen}] = true
			}
			if nw > ne {
				set[[5]int{nw, ne, r.Intn(3), r.Intn(3), tyellow}] = true
			}
		}
	}
	for nw := range 8 {
		for sw := range 3 {
			set[[5]int{nw, r.Intn(8), sw, r.Intn(3), lred}] = true
			set[[5]int{nw, r.Intn(8), sw, r.Intn(3), lyellow}] = true
			set[[5]int{nw, r.Intn(8), r.Intn(3), sw, ddred}] = true
			set[[5]int{nw, r.Intn(8), r.Intn(3), sw, ddyellow}] = true
		}
	}
	for sw := range 3 {
		for se := range 3 {
			if sw >= se {
				set[[5]int{r.Intn(8), r.Intn(8), sw, se, bgreen}] = true
			}
			if sw > se {
				set[[5]int{r.Intn(8), r.Intn(8), sw, se, byellow}] = true
			}
			if sw == se {
				set[[5]int{r.Intn(8), r.Intn(8), sw, se, bblue}] = true
			}
		}
	}
	return set
}

func trial(counts []int, diagrams map[[5]int]bool, r *rand.Rand) {
	connector := r.Intn(_last_connector)
	if diagrams[[5]int{r.Intn(9), r.Intn(9), r.Intn(4), r.Intn(4), connector}] {
		counts[connector]++
	}
}

func simulate(niter int, diagrams map[[5]int]bool, r *rand.Rand) {
	ch := make(chan []int)
	for range runtime.NumCPU() {
		go func(r *rand.Rand) {
			counts := make([]int, _last_connector)
			for range niter {
				trial(counts, diagrams, r)
			}
			ch <- counts
		}(rand.New(rand.NewSource(int64(r.Uint64()))))
	}
	sums := [_last_connector]int{}
	for range runtime.NumCPU() {
		counts := <-ch
		for i, count := range counts {
			sums[i] += count
		}
	}
	fmt.Printf("%d\n", sums)
	ps := [_last_connector]float64{}
	for i, sum := range sums {
		ps[i] = float64(sum) / float64(runtime.NumCPU()*niter)
	}
	fmt.Printf("%f\n", ps)
	p := [5]float64{}
	p[1] = ps[tyellow] + ps[lyellow] + ps[ddyellow] + ps[duyellow] + ps[ryellow] + ps[byellow]
	fmt.Printf("p1=%f=%f+%f+%f+%f+%f+%f\n", p[1], ps[tyellow], ps[lyellow], ps[ddyellow], ps[duyellow], ps[ryellow], ps[byellow])
	p[2] = ps[lred] + ps[ddred] + ps[dured] + ps[rred]
	fmt.Printf("p2=%f=%f+%f+%f+%f\n", p[2], ps[lred], ps[ddred], ps[dured], ps[rred])
	p[3] = ps[tgreen] + ps[bgreen]
	fmt.Printf("p3=%f=%f+%f\n", p[3], ps[tgreen], ps[bgreen])
	p[4] = ps[lblue] + ps[ddblue] + ps[dublue] + ps[rblue] + ps[bblue]
	fmt.Printf("p4=%f=%f+%f+%f+%f+%f\n", p[4], ps[lblue], ps[ddblue], ps[dublue], ps[rblue], ps[bblue])
	fmt.Printf("%f\n", 1/(p[1]+math.Sqrt(2)*p[2]+math.Sqrt(3)*p[3]+math.Sqrt(6)*p[4]))
}

func main() {
	const seed = 20261030
	r := rand.New(rand.NewSource(seed))
	diagrams := make_diagrams(r)
	fmt.Printf("%d\n", len(diagrams))

	const niter = 50000000
	simulate(niter, diagrams, r)
}
