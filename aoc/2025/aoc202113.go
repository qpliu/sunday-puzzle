package main

import (
	"bytes"
)

func init() {
	Register(&aoc202113{
		AOC: AOC{
			Day:           13,
			InputFilename: "../2021/input/13.txt",
			Tests: []Test{
				Test{
					Input: `6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5
`,
					Part1: "17",
					Part2: "",
				},
			},
		},
	})
}

type aoc202113 struct {
	AOC
}

func (aoc *aoc202113) parse(input *Input) (map[XY]bool, []int) {
	dots := map[XY]bool{}
	for {
		if input.Skip("\n\nfold along ") {
			break
		}
		x, _ := input.Int()
		y, _ := input.Int()
		dots[XY{x, y}] = true
	}
	folds := []int{}
	for {
		if input.Skip("x=") {
			x, _ := input.Int()
			folds = append(folds, x)
		} else if input.Skip("y=") {
			y, _ := input.Int()
			folds = append(folds, -y)
		}
		if !input.Skip("\nfold along ") {
			break
		}
	}
	return dots, folds
}

func (aoc *aoc202113) fold(dots map[XY]bool, fold int) {
	if fold > 0 {
		for xy := range dots {
			if xy[0] > fold {
				delete(dots, xy)
				dots[XY{2*fold - xy[0], xy[1]}] = true
			}
		}
	} else {
		for xy := range dots {
			if xy[1] > -fold {
				delete(dots, xy)
				dots[XY{xy[0], -2*fold - xy[1]}] = true
			}
		}
	}
}

func (aoc *aoc202113) Part1(input *Input) string {
	dots, folds := aoc.parse(input)
	aoc.fold(dots, folds[0])
	return IntResult(len(dots))
}

func (aoc *aoc202113) Part2(input *Input) string {
	dots, folds := aoc.parse(input)
	for _, fold := range folds {
		aoc.fold(dots, fold)
	}
	code := []string{}
	for y := range 6 {
		buf := bytes.Buffer{}
		for x := range 8 * 5 {
			if dots[XY{x, y}] {
				buf.WriteByte('#')
			} else {
				buf.WriteByte('.')
			}
		}
		code = append(code, buf.String())
	}
	return OCR4x6(code)
}
