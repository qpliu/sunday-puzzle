package main

import (
	"fmt"
	"os"
)

func readInput() []int {
	file, err := os.Open("input/05.txt")
	if err != nil {
		panic(err.Error())
	}
	defer file.Close()
	result := []int{}
	for {
		var i int
		if n, _ := fmt.Fscanf(file, "%d\n", &i); n == 0 {
			return result
		}
		result = append(result, i)
	}
}

func jumps(offsets []int) int {
	nsteps := 0
	i := 0
	for {
		if i < 0 || i >= len(offsets) {
			return nsteps
		}
		nexti := i + offsets[i]
		nsteps++
		offsets[i]++
		i = nexti
	}
}

func jumps2(offsets []int) int {
	nsteps := 0
	i := 0
	for {
		if i < 0 || i >= len(offsets) {
			return nsteps
		}
		nexti := i + offsets[i]
		nsteps++
		if offsets[i] >= 3 {
			offsets[i]--
		} else {
			offsets[i]++
		}
		i = nexti
	}
}

func main() {
	if n := jumps([]int{0, 3, 0, 1, -3}); n != 5 {
		panic(fmt.Sprintf("%d != 5", n))
	}
	if n := jumps2([]int{0, 3, 0, 1, -3}); n != 10 {
		panic(fmt.Sprintf("%d != 10", n))
	}
	println(jumps(readInput()))
	println(jumps2(readInput()))
}
