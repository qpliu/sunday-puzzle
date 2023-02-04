package main

import (
	"fmt"
	"os"
)

func scores(n int) []int {
	r := make([]int, n)
	r[0] = 3
	r[1] = 7
	e1 := 0
	e2 := 1
	i := 2
	for i < n {
		combine := r[e1] + r[e2]
		new1 := (combine / 10) % 10
		if new1 > 0 {
			r[i] = new1
			i++
			if i >= n {
				break
			}
		}
		new2 := combine % 10
		r[i] = new2
		i++
		e1 = (e1 + 1 + r[e1]) % i
		e2 = (e2 + 1 + r[e2]) % i
	}
	return r
}

func last10(r []int) string {
	return fmt.Sprintf("%d%d%d%d%d%d%d%d%d%d", r[len(r)-10], r[len(r)-9], r[len(r)-8], r[len(r)-7], r[len(r)-6], r[len(r)-5], r[len(r)-4], r[len(r)-3], r[len(r)-2], r[len(r)-1])
}

func part2(digits []int) int {
	r := make([]int, 2, 100000)
	r[0] = 3
	r[1] = 7
	e1 := 0
	e2 := 1
	i := 2
	for {
		combine := r[e1] + r[e2]
		new1 := (combine / 10) % 10
		if new1 > 0 {
			r = append(r, new1)
			i++
			if i >= len(digits) {
				found := true
				for j, d := range digits {
					if d != r[i-len(digits)+j] {
						found = false
						break
					}
				}
				if found {
					return i - len(digits)
				}
			}
		}
		new2 := combine % 10
		r = append(r, new2)
		i++
		if i >= len(digits) {
			found := true
			for j, d := range digits {
				if d != r[i-len(digits)+j] {
					found = false
					break
				}
			}
			if found {
				return i - len(digits)
			}
		}
		e1 = (e1 + 1 + r[e1]) % i
		e2 = (e2 + 1 + r[e2]) % i
	}
}

func main() {
	if len(os.Args) < 2 {
		if r := last10(scores(9 + 10)); r != "5158916779" {
			panic(r)
		}
		if r := last10(scores(5 + 10)); r != "0124515891" {
			panic(r)
		}
		if r := last10(scores(18 + 10)); r != "9251071085" {
			panic(r)
		}
		if r := last10(scores(2018 + 10)); r != "5941429882" {
			panic(r)
		}
		if n := part2([]int{5, 1, 5, 8, 9}); n != 9 {
			panic(fmt.Sprintf("%d", n))
		}
		if n := part2([]int{0, 1, 2, 4, 5}); n != 5 {
			panic(fmt.Sprintf("%d", n))
		}
		if n := part2([]int{9, 2, 5, 1, 0}); n != 18 {
			panic(fmt.Sprintf("%d", n))
		}
		if n := part2([]int{5, 9, 4, 1, 4}); n != 2018 {
			panic(fmt.Sprintf("%d", n))
		}
	} else {
		l := 0
		fmt.Sscanf(os.Args[1], "%d", &l)
		println(last10(scores(l + 10)))
		d := []int{}
		for _, c := range os.Args[1] {
			d = append(d, int(c-'0'))
		}
		println(part2(d))
	}
}
