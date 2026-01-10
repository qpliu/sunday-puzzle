package main

import (
	"math/bits"
	"runtime"
)

func init() {
	Register(&aoc202510{
		AOC: AOC{
			Day: 10,
			Tests: []Test{
				Test{
					Input: `[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
`,
					Part1: "7",
					Part2: "33",
				},
			},
		},
	})
}

type aoc202510 struct {
	AOC
}

type aoc202510_machine struct {
	lights   int
	buttons  []int
	joltages []int

	buttons2 [][]int
}

func (m aoc202510_machine) configure() int {
	minPresses := len(m.buttons)
	for i := range 1 << len(m.buttons) {
		npresses := bits.OnesCount(uint(i))
		if npresses >= minPresses {
			continue
		}
		lights := 0
		for j, b := range m.buttons {
			if (1<<j)&i != 0 {
				lights ^= b
			}
		}
		if lights == m.lights {
			minPresses = npresses
		}
	}
	return minPresses
}

func (aoc *aoc202510) parse(input *Input) Seq[aoc202510_machine] {
	return func(yield func(aoc202510_machine) bool) {
		for line, ok := input.Line(); ok; line, ok = input.Line() {
			in := InputString(line)
			in.Char()
			lights := 0
			bit := 1
			for ch, _ := in.Char(); ch != ']'; ch, _ = in.Char() {
				if ch == '#' {
					lights |= bit
				}
				bit <<= 1
			}
			var buttons []int
			var buttons2 [][]int
			for {
				w, _ := in.Word()
				if w[0] == '{' {
					if !yield(aoc202510_machine{
						lights:   lights,
						buttons:  buttons,
						joltages: InputString(w).Ints(),
						buttons2: buttons2,
					}) {
						return
					}
					break
				}
				button := 0
				button2 := []int{}
				for _, i := range InputString(w).Ints() {
					button |= 1 << i
					button2 = append(button2, i)
				}
				buttons = append(buttons, button)
				buttons2 = append(buttons2, button2)
			}
		}
	}
}

func (aoc *aoc202510) Part1(input *Input) string {
	in := make(chan aoc202510_machine)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for machine := range in {
				result += machine.configure()
			}
			out <- result
		}()
	}
	for machine := range aoc.parse(input) {
		in <- machine
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (m aoc202510_machine) configure2() int {
	matrix := make([][]int, len(m.joltages))
	maxPresses := make([]int, len(m.buttons))
	for i, rhs := range m.joltages {
		row := make([]int, len(m.buttons)+1)
		row[len(m.buttons)] = rhs
		matrix[i] = row
		for j, button := range m.buttons {
			if button&(1<<i) != 0 {
				maxPresses[j] = max(maxPresses[j], rhs)
				row[j] = 1
			}
		}
	}

	// to upper diagonal
	for i := range matrix {
		if i >= len(matrix[i])-1 {
			break
		}
		if matrix[i][i] != 0 {
			goto foundPivot
		}
		for j := i + 1; j < len(matrix); j++ {
			if matrix[j][i] != 0 {
				matrix[i], matrix[j] = matrix[j], matrix[i]
				goto foundPivot
			}
		}
		for j := i + 1; j < len(matrix[i])-1; j++ {
			if matrix[i][j] != 0 {
				maxPresses[i], maxPresses[j] = maxPresses[j], maxPresses[i]
				for k := range matrix {
					matrix[k][i], matrix[k][j] = matrix[k][j], matrix[k][i]
				}
				goto foundPivot
			}
		}
		continue
	foundPivot:
		for j := i + 1; j < len(matrix); j++ {
			ai := matrix[i][i]
			aj := matrix[j][i]
			for k := i; k < len(matrix[j]); k++ {
				matrix[j][k] = matrix[j][k]*ai - matrix[i][k]*aj
			}
		}
	}

	presses := 0
	// backsubstitute
	substitutionMade := true
	for substitutionMade {
		substitutionMade = false
		for i := len(matrix) - 1; i >= 0; i-- {
			column := -1
			for j := range len(matrix[i]) - 1 {
				if matrix[i][j] != 0 {
					if column == -1 {
						column = j
					} else {
						column = -2
						break
					}
				}
			}
			if column == -2 {
				continue
			} else if column == -1 {
				copy(matrix[i:], matrix[i+1:])
				matrix = matrix[:len(matrix)-1]
				continue
			}
			substitutionMade = true
			p := matrix[i][len(matrix[i])-1] / matrix[i][column]
			if p*matrix[i][column] != matrix[i][len(matrix[i])-1] {
				panic("bad input")
			}
			if p < 0 {
				panic("bad input")
			}
			if p > maxPresses[column] {
				panic("bad input")
			}
			presses += p
			copy(maxPresses[column:], maxPresses[column+1:])
			maxPresses = maxPresses[:len(maxPresses)-1]
			for i, row := range matrix {
				row[len(row)-1] -= row[column] * p
				copy(row[column:], row[column+1:])
				matrix[i] = row[:len(row)-1]
			}
		}
	}

	neqs := len(matrix)
	if neqs == 0 {
		return presses
	}
	nvars := len(matrix[0]) - 1
	best := 0
	for _, maxPress := range maxPresses {
		best += maxPress
	}
	sol := make([]int, nvars)
	sol[neqs] = -1
bruteForceLoop:
	for {
		more := false
		for i := neqs; i < nvars; i++ {
			sol[i]++
			if sol[i] <= maxPresses[i] {
				more = true
				break
			}
			sol[i] = 0
		}
		if !more {
			return presses + best
		}
		for i := neqs - 1; i >= 0; i-- {
			rhs := matrix[i][nvars]
			for j := i + 1; j < nvars; j++ {
				rhs -= matrix[i][j] * sol[j]
			}
			sol[i] = rhs / matrix[i][i]
			if sol[i] < 0 || sol[i]*matrix[i][i] != rhs {
				continue bruteForceLoop
			}
		}
		sum := 0
		for _, s := range sol {
			sum += s
		}
		best = min(best, sum)
	}
}

func (aoc *aoc202510) Part2(input *Input) string {
	in := make(chan aoc202510_machine)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for machine := range in {
				result += machine.configure2()
			}
			out <- result
		}()
	}

	for machine := range aoc.parse(input) {
		in <- machine
	}
	close(in)

	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
