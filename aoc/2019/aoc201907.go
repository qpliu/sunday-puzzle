package main

import (
	"fmt"
	"os"
	"sync"
)

func run(ic *intcode) (int64, [5]int64) {
	maxOutput := int64(0)
	maxSequence := [5]int64{}
	for i0 := int64(0); i0 < 5; i0++ {
		for i1 := int64(0); i1 < 5; i1++ {
			if i1 == i0 {
				continue
			}
			for i2 := int64(0); i2 < 5; i2++ {
				if i2 == i1 || i2 == i0 {
					continue
				}
				for i3 := int64(0); i3 < 5; i3++ {
					if i3 == i2 || i3 == i1 || i3 == i0 {
						continue
					}
					for i4 := int64(0); i4 < 5; i4++ {
						if i4 == i3 || i4 == i2 || i4 == i1 || i4 == i0 {
							continue
						}
						ic.init()
						out := ic.interp([]int64{i0, 0})
						ic.init()
						out = ic.interp([]int64{i1, out[0]})
						ic.init()
						out = ic.interp([]int64{i2, out[0]})
						ic.init()
						out = ic.interp([]int64{i3, out[0]})
						ic.init()
						out = ic.interp([]int64{i4, out[0]})
						if out[0] > maxOutput || maxSequence == [5]int64{} {
							maxOutput = out[0]
							maxSequence = [5]int64{i0, i1, i2, i3, i4}
						}
					}
				}
			}
		}
	}
	return maxOutput, maxSequence
}

func test(input string, expectedOut int64, expectedSeq [5]int64) {
	ic := parseString(input)
	out, seq := run(ic)
	if out != expectedOut || seq != expectedSeq {
		fmt.Printf("%s\n", input)
		fmt.Printf("%d %d != %d %d\n", out, seq[:], expectedOut, expectedSeq[:])
	}
}

func run2(ic *intcode) (int64, [5]int64) {
	maxOutput := int64(0)
	maxSequence := [5]int64{}
	for i0 := int64(5); i0 < 10; i0++ {
		for i1 := int64(5); i1 < 10; i1++ {
			if i1 == i0 {
				continue
			}
			for i2 := int64(5); i2 < 10; i2++ {
				if i2 == i1 || i2 == i0 {
					continue
				}
				for i3 := int64(5); i3 < 10; i3++ {
					if i3 == i2 || i3 == i1 || i3 == i0 {
						continue
					}
					for i4 := int64(5); i4 < 10; i4++ {
						if i4 == i3 || i4 == i2 || i4 == i1 || i4 == i0 {
							continue
						}
						sequence := [5]int64{i0, i1, i2, i3, i4}
						output := runLoop(ic, sequence)
						if output > maxOutput {
							maxOutput = output
							maxSequence = sequence
						}
					}
				}
			}
		}
	}
	return maxOutput, maxSequence
}

func runLoop(baseIC *intcode, seq [5]int64) int64 {
	var wg sync.WaitGroup
	wg.Add(5)

	var result int64
	channels := [5]chan int64{make(chan int64,1),make(chan int64,1),make(chan int64,1),make(chan int64,1),make(chan int64,1)}
	for i := 0; i < 5; i++ {
		channels[i] <- seq[i]
		go func(ix int) {
			ic := &intcode{}
			ic.initFrom(baseIC)
			ic.interpIO(func() (bool, int64) {
				return false, <-channels[ix]
			}, func(out int64) bool {
				if ix == 4 {
					result = out
				}
				channels[(ix+1)%5] <- out
				return false
			})
			wg.Done()
		}(i)
	}
	channels[0] <- 0
	wg.Wait()
	return result
}

func test2(input string, expectedOut int64, expectedSeq [5]int64) {
	ic := parseString(input)
	out, seq := run2(ic)
	if out != expectedOut || seq != expectedSeq {
		fmt.Printf("%s\n", input)
		fmt.Printf("%d %d != %d %d\n", out, seq[:], expectedOut, expectedSeq[:])
	}
}

func main() {
	if len(os.Args) < 2 {
		test("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", 43210, [5]int64{4, 3, 2, 1, 0})
		test("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", 54321, [5]int64{0, 1, 2, 3, 4})
		test("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", 65210, [5]int64{1, 0, 4, 3, 2})

		test2("3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5", 139629729, [5]int64{9,8,7,6,5})
		test2("3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10", 18216, [5]int64{9,7,8,5,6})
	} else {
		ic := parseFile("input/07.txt")

		// Part 1
		out, seq := run(ic)
		fmt.Printf("%d %d\n", out, seq[:])

		// Part 2
		out, seq = run2(ic)
		fmt.Printf("%d %d\n", out, seq[:])
	}
}
