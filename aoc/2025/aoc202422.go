package main

import (
	"runtime"
)

func init() {
	Register(&aoc202422{
		AOC: AOC{
			Day:           22,
			InputFilename: "../2024/input/22.txt",
			Tests: []Test{
				Test{
					Input: `1
10
100
2024
`,
					Part1: "37327623",
					Part2: "",
				},
				Test{
					Input: `1
2
3
2024
`,
					Part1: "",
					Part2: "23",
				},
			},
		},
	})
}

type aoc202422 struct {
	AOC
}

func (aoc *aoc202422) next(n int) int {
	n ^= n << 6 & 0xffffff
	n ^= n >> 5
	n ^= n << 11
	return n & 0xffffff
}

func (aoc *aoc202422) worker(in chan int, out chan int) {
	total := 0
	for n := range in {
		for range 2000 {
			n = aoc.next(n)
		}
		total += n
	}
	out <- total
}

func (aoc *aoc202422) Part1(input *Input) string {
	in := make(chan int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go aoc.worker(in, out)
	}
	for n, ok := input.Int(); ok; n, ok = input.Int() {
		in <- n
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202422) worker2(in chan int, out chan map[[4]int]int) {
	results := map[[4]int]int{}
	for n := range in {
		dp := [4]int{}
		lastp := 0
		result := map[[4]int]int{}
		for range 3 {
			n = aoc.next(n)
			dp = [4]int{dp[1], dp[2], dp[3], n%10 - lastp}
			lastp = n % 10
		}
		for range 1997 {
			n = aoc.next(n)
			dp = [4]int{dp[1], dp[2], dp[3], n%10 - lastp}
			lastp = n % 10
			if _, ok := result[dp]; !ok {
				result[dp] = n % 10
			}
		}
		for k, v := range result {
			results[k] += v
		}
	}
	out <- results
}

func (aoc *aoc202422) Part2(input *Input) string {
	in := make(chan int)
	out := make(chan map[[4]int]int)
	for range runtime.NumCPU() {
		go aoc.worker2(in, out)
	}
	for n, ok := input.Int(); ok; n, ok = input.Int() {
		in <- n
	}
	close(in)
	results := map[[4]int]int{}
	for range runtime.NumCPU() {
		for k, v := range <-out {
			results[k] += v
		}
	}
	result := 0
	for _, v := range results {
		result = max(result, v)
	}
	return IntResult(result)
}
