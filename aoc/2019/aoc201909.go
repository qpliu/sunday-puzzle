package main

import (
	"fmt"
	"os"
)

func test(input string, expectedOut []int64) {
	ic := parseString(input)
	ic.init()
	out := ic.interp([]int64{})
	if len(out) != len(expectedOut) {
		panic(fmt.Sprintf("%d != %d", len(out), len(expectedOut)))
	}
	for i := range out {
		if out[i] != expectedOut[i] {
			panic(fmt.Sprintf("%d: %d != %d", i, out[i], expectedOut[i]))
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		test("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99", []int64{109, 1, 204, -1, 1001, 100, 1, 100, 1008, 100, 16, 101, 1006, 101, 0, 99})
		test("1102,34915192,34915192,7,4,7,99,0", []int64{1219070632396864})
		test("104,1125899906842624,99", []int64{1125899906842624})
	} else {
		ic := parseFile("input/09.txt")

		// Part 1
		ic.init()
		out := ic.interp([]int64{1})
		fmt.Printf("%d\n", out)

		// Part 2
		ic.init()
		out = ic.interp([]int64{2})
		fmt.Printf("%d\n", out)
	}
}
