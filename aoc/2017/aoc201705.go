/*
--- Day 5: A Maze of Twisty Trampolines, All Alike ---

An urgent interrupt arrives from the CPU: it's trapped in a maze of jump
instructions, and it would like assistance from any programs with spare cycles
to help find the exit.

The message includes a list of the offsets for each jump. Jumps are relative:
-1 moves to the previous instruction, and 2 skips the next one. Start at the
first instruction in the list. The goal is to follow the jumps until one leads
outside the list.

In addition, these instructions are a little strange; after each jump, the
offset of that instruction increases by 1. So, if you come across an offset of
3, you would move three instructions forward, but change it to a 4 for the next
time it is encountered.

For example, consider the following list of jump offsets:

| 0
| 3
| 0
| 1
| -3

Positive jumps ("forward") move downward; negative jumps move upward. For
legibility in this example, these offset values will be written all on one
line, with the current instruction marked in parentheses. The following steps
would be taken before an exit is found:

 - (0) 3  0  1  -3  - before we have taken any steps.
 - (1) 3  0  1  -3  - jump with offset 0 (that is, don't jump at all).
   Fortunately, the instruction is then incremented to 1.
 -  2 (3) 0  1  -3  - step forward because of the instruction we just modified.
   The first instruction is incremented again, now to 2.
 -  2  4  0  1 (-3) - jump all the way to the end; leave a 4 behind.
 -  2 (4) 0  1  -2  - go back to where we just were; increment -3 to -2.
 -  2  5  0  1  -2  - jump 4 steps forward, escaping the maze.

In this example, the exit is reached in 5 steps.

How many steps does it take to reach the exit?
-}
*/

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
