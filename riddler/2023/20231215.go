package main

import (
	"fmt"
	"math/rand"
)

const (
	hhh = iota
	hht
	hth
	htt
	thh
	tht
	tth
	ttt
)

func next(n int, heads bool) int {
	if heads {
		switch n {
		case hhh: return hhh
		case hht: return hth
		case hth: return thh
		case htt: return tth
		case thh: return hhh
		case tht: return hth
		case tth: return thh
		case ttt: return tth
		default:
			panic("")
		}
	} else {
		switch n {
		case hhh: return hht
		case hht: return htt
		case hth: return tht
		case htt: return ttt
		case thh: return hht
		case tht: return htt
		case tth: return tht
		case ttt: return ttt
		default:
			panic("")
		}
	}
}

func trial(jtarget, ktarget int, r *rand.Rand) bool {
	j := r.Intn(8)
	k := r.Intn(8)
	for {
		if j == jtarget {
			if k != ktarget {
				return false
			}
		} else if k == ktarget {
			return true
		}
		switch r.Intn(4) {
		case 0: // HH
			j = next(j, true)
			k = next(k, true)
		case 1: // TH
			j = next(j, false)
			k = next(k, true)
		case 2: // HT
			j = next(j, true)
			k = next(k, false)
		case 3: // TT
			j = next(j, false)
			k = next(k, false)
		default:
			panic("")
		}
	}
}

func simulate(jtarget, ktarget int, n int, r *rand.Rand) int {
	count := 0
	for i := 0; i < n; i++ {
		if trial(jtarget, ktarget, r) {
			count++
		}
	}
	return count
}

func main() {
	const n = 1000000
	const seed = 71835290
	count := simulate(ttt, tth, n, rand.New(rand.NewSource(seed)))
	fmt.Printf("%d %f\n", count, float64(count)/n)
}
