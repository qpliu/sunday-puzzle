package main

import (
	"os"
	"strconv"
)

const nmax = 30000000

var history [nmax]int

func clearHistory() {
	for i := range history {
		history[i] = 0
	}
}

func play(n int, start []int) int {
	i := 0
	num := 0
	for _, next := range start {
		history[num] = i
		num = next
		i++
	}
	for i < n {
		next := history[num]
		history[num] = i
		if next != 0 {
			num = i - next
		} else {
			num = 0
		}
		i++
	}
	return num
}

func main() {
	if len(os.Args) < 2 {
		println(play(2020, []int{0,3,6}), 436)
		clearHistory()
		println(play(2020, []int{1,3,2}), 1)
		clearHistory()
		println(play(2020, []int{2,1,3}), 10)
		clearHistory()
		println(play(2020, []int{1,2,3}), 27)
		clearHistory()
		println(play(2020, []int{2,3,1}), 78)
		clearHistory()
		println(play(2020, []int{3,2,1}), 438)
		clearHistory()
		println(play(2020, []int{3,1,2}), 1836)
		clearHistory()
		println(play(30000000, []int{0,3,6}), 175594)
		clearHistory()
		println(play(30000000, []int{1,3,2}), 2578)
		clearHistory()
		println(play(30000000, []int{2,1,3}), 3544142)
		clearHistory()
		println(play(30000000, []int{1,2,3}), 261214)
		clearHistory()
		println(play(30000000, []int{2,3,1}), 6895259)
		clearHistory()
		println(play(30000000, []int{3,2,1}), 18)
		clearHistory()
		println(play(30000000, []int{3,1,2}), 362)
	} else {
		start := []int{}
		for _, arg := range os.Args[1:] {
			n, err := strconv.Atoi(arg)
			if err != nil {
				panic(err.Error())
			}
			start = append(start, n)
		}
		println(play(2020, start))
		clearHistory()
		println(play(30000000, start))
	}
}
