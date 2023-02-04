package main

import (
	"os"
)

func move(next []int, current int) int {
	move1 := next[current]
	move2 := next[move1]
	move3 := next[move2]
	nextCurrent := next[move3]
	target := current-1
	for {
		if target <= 0 {
			target = len(next)-1
		}
		if target != move1 && target != move2 && target != move3 {
			break
		}
		target--
	}

	next[current] = nextCurrent
	next[move3] = next[target]
	next[target] = move1
	return nextCurrent
}

func run(initCups []int, size, moves int) []int {
	next := make([]int, size+1)
	current := initCups[0]
	for i := range next {
		next[i] = i+1
	}
	next[size] = current
	last := 0
	for _, cup := range initCups {
		next[last] = cup
		last = cup
	}
	if len(initCups) >= size {
		next[last] = current
	} else {
		next[last] = len(initCups) + 1
	}
	for i := 0; i < moves; i++ {
		current = move(next, current)
	}
	return next
}

func printCups(next []int) {
	n := 1
	for i := 1; i < len(next); i++ {
		print(" ",n)
		n = next[n]
	}
	println()
}

func main() {
	cups := run([]int{3,8,9,1,2,5,4,6,7}, 9, 0)
	print("0:")
	printCups(cups)
	cups = run([]int{3,8,9,1,2,5,4,6,7}, 9, 10)
	print("10:")
	printCups(cups)
	cups = run([]int{3,8,9,1,2,5,4,6,7}, 9, 100)
	print("100:")
	printCups(cups)
	cups = run([]int{3,8,9,1,2,5,4,6,7}, 1000000, 10000000)
	println("10000000:", cups[1], cups[cups[1]], cups[1]*cups[cups[1]])
	if len(os.Args) > 1 {
		start := []int{}
		for _, b := range([]byte(os.Args[1])) {
			start = append(start, int(b-'0'))
		}
		cups = run(start, 1000000, 10000000)
		println("10000000:", cups[1], cups[cups[1]], cups[1]*cups[cups[1]])
	}
}
