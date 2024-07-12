package main

import (
	"fmt"
	"math/rand"
)

func game(target int, r *rand.Rand) (int, int, bool) {
	points1 := 0
	points2 := 0
	for {
		if r.Intn(2) == 0 {
			points1++
		} else {
			points2++
		}
		if points1 > points2+1 && points1 >= target {
			return points1, points2, true
		} else if points2 > points1+1 && points2 >= target {
			return points1, points2, false
		}
	}
}

func set(r *rand.Rand) (int, int, int, bool) {
	points1 := 0
	points2 := 0
	games1 := 0
	games2 := 0
	for {
		p1, p2, g := game(4, r)
		points1 += p1
		points2 += p2
		if g {
			games1++
		} else {
			games2++
		}
		if games1 >= 6 && games1 > games2+1 {
			return points1, points2, games1 + games2, true
		} else if games2 >= 6 && games2 > games1+1 {
			return points1, points2, games1 + games2, false
		} else if games1 == 6 && games2 == 6 {
			p1, p2, g = game(7, r)
			return points1 + p1, points2 + p2, 13, g
		}
	}
}

func match(r *rand.Rand) (int, int, bool) {
	points1 := 0
	points2 := 0
	sets1 := 0
	sets2 := 0
	for {
		p1, p2, _, g := set(r)
		points1 += p1
		points2 += p2
		if g {
			sets1++
		} else {
			sets2++
		}
		if sets1 >= 2 {
			return points1, points2, true
		} else if sets2 >= 2 {
			return points1, points2, false
		}
	}
}

func simulate(n int, r *rand.Rand) int {
	count := 0
	for _ = range n {
		points1, points2, g := match(r)
		if (g && points1 < points2) || (!g && points2 < points1) {
			count++
		}
	}
	return count
}

func ecmatch(n int, r *rand.Rand) [103]int {
	counts := [103]int{}
	for _ = range n {
		p1, p2, g := match(r)
		if g {
			counts[p1-p2+44]++
		} else {
			counts[p2-p1+44]++
		}
	}
	return counts
}

func ecset(n int, r *rand.Rand) ([49]int, [49][8]int) {
	counts := [49]int{}
	bygames := [49][8]int{}
	for _ = range n {
		p1, p2, gms, g := set(r)
		if g {
			counts[p1-p2+24]++
			bygames[p1-p2+24][gms-6]++
		} else {
			counts[p2-p1+24]++
			bygames[p2-p1+24][gms-6]++
		}
	}
	return counts, bygames
}

func ecgame(n int, r *rand.Rand) [3]int {
	counts := [3]int{}
	for _ = range n {
		p1, p2, g := game(4, r)
		if g {
			counts[p1-p2-2]++
		}
	}
	return counts
}

func ectiebreak(n int, r *rand.Rand) [6]int {
	counts := [6]int{}
	for _ = range n {
		p1, p2, g := game(7, r)
		if g {
			counts[p1-p2-2]++
		}
	}
	return counts
}

func main() {
	const n = 1000000
	const seed = 731735
	r := rand.New(rand.NewSource(seed))
	count := simulate(n, r)
	fmt.Printf("%d/%d %f\n", count, n, float64(count)/n)
	count = 0
	for i, c := range ecmatch(n, r) {
		if c == 0 {
			continue
		}
		fmt.Printf("match: %d %d/%d %f\n", i-44, c, n, float64(c)/n)
		if i < 44 {
			count += c
		}
	}
	fmt.Printf("%d/%d %f\n", count, n, float64(count)/n)
	{
		bypoints, bygames := ecset(n, r)
		for i, c := range bypoints {
			if c == 0 {
				continue
			}
			fmt.Printf("set: %d %d/%d %f\n", i-24, c, n, float64(c)/n)
		}
		for g := range 8 {
			score := fmt.Sprintf("6-%d", g)
			if g == 7 {
				score = "7-6"
			} else if g == 6 {
				score = "7-5"
			}
			for i, gms := range bygames {
				if gms[g] == 0 {
					continue
				}
				fmt.Printf("  %s: %d %d/%d %f\n", score, i-24, gms[g], n, float64(gms[g])/n)
			}
		}
	}
	for i, c := range ecgame(n, r) {
		fmt.Printf("game: %d %d/%d %f\n", i+2, c, n, float64(c)/n)
	}
	for i, c := range ectiebreak(n, r) {
		fmt.Printf("tiebreak: %d %d/%d %f\n", i+2, c, n, float64(c)/n)
	}
}
