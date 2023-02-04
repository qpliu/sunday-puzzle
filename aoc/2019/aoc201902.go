package main

import (
	"fmt"
	"os"
)

func test(input string, result []int64) {
	ic := parseString(input)
	ic.init()
	ic.interp([]int64{})
	if len(ic.mem) != len(result) {
		panic(fmt.Sprintf("%d != %d", len(ic.mem), len(result)))
	}
	for i := range ic.mem {
		if ic.mem[i] != result[i] {
			panic(fmt.Sprintf("%d: %d != %d", i, ic.mem[i], result[i]))
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		test("1,0,0,0,99", []int64{2, 0, 0, 0, 99})
		test("2,3,0,3,99", []int64{2, 3, 0, 6, 99})
		test("2,4,4,5,99,0", []int64{2, 4, 4, 5, 99, 9801})
		test("1,1,1,4,99,5,6,0,99", []int64{30, 1, 1, 4, 2, 5, 6, 0, 99})
		test("1,9,10,3,2,3,11,0,99,30,40,50", []int64{3500, 9, 10, 70, 2, 3, 11, 0, 99, 30, 40, 50})
	} else {
		ic := parseFile("input/02.txt")

		// Part 1
		ic.init()
		ic.mem[1] = 12
		ic.mem[2] = 2
		ic.interp([]int64{})
		println(ic.mem[0])

		// Part 2
	outer:
		for n := 0; n <= 128; n++ {
			for v := 0; v <= 128; v++ {
				ic.init()
				ic.mem[1] = int64(n)
				ic.mem[2] = int64(v)
				ic.interp([]int64{})
				if ic.mem[0] == 19690720 { // someone's birthday?
					println(n, v, n*100+v)
					break outer
				}
			}
		}
	}
}
