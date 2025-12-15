package main

import (
	"unicode"
)

func init() {
	Register(&aoc202506{
		AOC: AOC{
			Day: 6,
			Tests: []Test{
				Test{
					Input: `123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
`,
					Part1: "4277556",
					Part2: "3263827",
				},
			},
		},
	})
}

type aoc202506 struct {
	AOC
}

func (aoc *aoc202506) parse1(input *Input) ([][]int, []string) {
	var rows [][]int
	for {
		rowString, _ := input.Line()
		rowInput := InputString(rowString)
		var row []int
		for i, ok := rowInput.Int(); ok; i, ok = rowInput.Int() {
			row = append(row, i)
		}
		if row != nil {
			rows = append(rows, row)
		} else {
			rowInput.Reset()
			var ops []string
			for op, ok := rowInput.Word(); ok; op, ok = rowInput.Word() {
				ops = append(ops, op)
			}
			return rows, ops
		}
	}
}

func (aoc *aoc202506) Part1(input *Input) string {
	rows, ops := aoc.parse1(input)
	result := 0
	for col, op := range ops {
		switch op {
		case "+":
			sum := 0
			for _, row := range rows {
				sum += row[col]
			}
			result += sum
		case "*":
			prod := 1
			for _, row := range rows {
				prod *= row[col]
			}
			result += prod
		}
	}
	return IntResult(result)
}

func (aoc *aoc202506) Part2(input *Input) string {
	width, height, grid := input.Grid()
	result := 0
	subresult := 0
	op := 0
	for col := range width + 1 {
		num := 0
		for row := range height - 1 {
			d := grid[[2]int{col, row}]
			if unicode.IsDigit(rune(d)) {
				num = num*10 + int(d-'0')
			}
		}
		// none of the numbers are 0
		if num == 0 {
			result += subresult
			op = 0
			continue
		}
		switch grid[[2]int{col, height - 1}] {
		case '+':
			op = '+'
			subresult = 0
		case '*':
			op = '*'
			subresult = 1
		}
		switch op {
		case '+':
			subresult += num
		case '*':
			subresult *= num
		}
	}
	return IntResult(result)
}
