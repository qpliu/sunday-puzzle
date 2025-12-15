package main

func init() {
	Register(&aoc202512{
		AOC: AOC{
			Day: 12,
			Tests: []Test{
				Test{
					Input: `0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2
`,
					Part1: "",
					Part2: "",
				},
			},
		},
	})
}

type aoc202512 struct {
	AOC
}

type aoc202512_shape struct {
	shape string
	units int
}

func (aoc *aoc202512) parse(input *Input) ([]aoc202512_shape, [][]int) {
	var shapes []aoc202512_shape
	lastParagraph := ""
	for paragraph, ok := input.Paragraph(); ok; paragraph, ok = input.Paragraph() {
		if lastParagraph != "" {
			units := 0
			for _, c := range lastParagraph {
				if c == '#' {
					units++
				}
			}
			shapes = append(shapes, aoc202512_shape{lastParagraph, units})
		}
		lastParagraph = paragraph
	}
	var trees [][]int
	input = InputString(lastParagraph)
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		trees = append(trees, InputString(line).Ints())
	}
	return shapes, trees
}

func (aoc *aoc202512) fits(shapes []aoc202512_shape, tree []int) bool {
	width := tree[0]
	height := tree[1]
	npresents := 0
	nunits := 0
	for i, n := range tree[2:] {
		npresents += n
		nunits += n * shapes[i].units
	}
	if npresents <= (width/3)*(height/3) {
		return true
	}
	if nunits > width*height {
		return false
	}
	panic("can't tell without packing")
}

func (aoc *aoc202512) Part1(input *Input) string {
	shapes, trees := aoc.parse(input)
	result := 0
	for _, tree := range trees {
		if aoc.fits(shapes, tree) {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202512) Part2(input *Input) string {
	return ""
}
