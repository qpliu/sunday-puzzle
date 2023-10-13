package main

import (
	"fmt"
	"math/rand"
)

var traceOn bool = false

func game(a, b []int) bool {
	aleads := true
	for {
		a, b, aleads = round(a, b, aleads)
		if len(a) == 0 {
			return false
		}
		if len(b) == 0 {
			return true
		}
	}
}

func trace(s string) {
	if traceOn {
		println(s)
	}
}

func round(a, b []int, aleads bool) ([]int, []int, bool) {
	var played []int
	for {
		if aleads {
			if len(a) == 0 {
				trace("a is out of cards and loses")
				return a, append(b, played...), false
			}
			trace(fmt.Sprintf("a plays %d", a[0]))
			if len(played) > 0 && a[0] == played[len(played)-1] {
				trace("a wins slap")
				return append(append(a[1:], played...), a[0]), b, true
			}
			atop := a[0]
			played = append(played, atop)
			a = a[1:]
			if atop <= 10 {
				aleads = false
				continue
			}
			beatsChallenge := false
			for i := 10; i < atop; i++ {
				if len(b) == 0 {
					trace("b is out of cards and loses")
					return append(a, played...), b, true
				}
				trace(fmt.Sprintf("b plays %d", b[0]))
				btop := b[0]
				if btop == played[len(played)-1] {
					trace("a wins slap")
					return append(append(a, played...), btop), b[1:], true
				}
				played = append(played, btop)
				b = b[1:]
				if btop > 10 {
					beatsChallenge = true
					break
				}
			}
			if !beatsChallenge {
				trace("a wins round")
				return append(a, played...), b, true
			}
		} else {
			if len(b) == 0 {
				trace("b is out of cards and loses")
				return append(a, played...), b, true
			}
			if len(played) > 0 && b[0] == played[len(played)-1] {
				trace("a wins slap")
				return append(append(a, played...), b[0]), b[1:], true
			}
			trace(fmt.Sprintf("b plays %d", b[0]))
			btop := b[0]
			played = append(played, btop)
			b = b[1:]
			if btop <= 10 {
				aleads = true
				continue
			}
			beatsChallenge := false
			for i := 10; i < btop; i++ {
				if len(a) == 0 {
					trace("a is out of cards and loses")
					return a, append(b, played...), false
				}
				trace(fmt.Sprintf("a plays %d", a[0]))
				atop := a[0]
				if atop == played[len(played)-1] {
					trace("a wins slap")
					return append(append(a, played...), atop), b, true
				}
				played = append(played, atop)
				a = a[1:]
				if atop > 10 {
					beatsChallenge = true
					break
				}
			}
			if !beatsChallenge {
				trace("b wins round")
				return a, append(b, played...), false
			}
		}
	}
}

func trial(r *rand.Rand) bool {
	deck := []int{14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
		14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
		14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13,
		14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13}
	r.Shuffle(len(deck), func(i, j int) { deck[i], deck[j] = deck[j], deck[i] })
	return game(deck[:26], deck[26:])
}

func simulate(n int, r *rand.Rand) int {
	w := 0
	for i := 0; i < n; i++ {
		if trial(r) {
			w++
		}
	}
	return w
}

func main() {
	const n = 2000000
	const seed = 30638799
	traceOn = n < 2
	w := simulate(n, rand.New(rand.NewSource(seed)))
	println(w, n, float64(w)/float64(n), float64(n-w)/float64(n))
}
