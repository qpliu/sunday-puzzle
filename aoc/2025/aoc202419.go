package main

func init() {
	Register(&aoc202419{
		AOC: AOC{
			Day:           19,
			InputFilename: "../2024/input/19.txt",
			Tests: []Test{
				Test{
					Input: `r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
`,
					Part1: "6",
					Part2: "16",
				},
			},
		},
	})
}

type aoc202419 struct {
	AOC
}

func (aoc *aoc202419) patterns(input *Input) []string {
	paragraph, _ := input.Paragraph()
	input = InputString(paragraph)
	pats := []string{}
	for pat, ok := input.Word(); ok; pat, ok = input.Word() {
		if pat[len(pat)-1] == ',' {
			pat = pat[:len(pat)-1]
		}
		pats = append(pats, pat)
	}
	return pats
}

func (aoc *aoc202419) possible(design string, pats []string, memo map[string]bool) bool {
	if p, ok := memo[design]; ok {
		return p
	}
	for _, pat := range pats {
		if len(pat) > len(design) {
			continue
		}
		if pat == design[:len(pat)] {
			d := design[len(pat):]
			if d == "" || aoc.possible(d, pats, memo) {
				memo[design] = true
				return true
			}
		}
	}
	memo[design] = false
	return false
}

func (aoc *aoc202419) Part1(input *Input) string {
	pats := aoc.patterns(input)
	memo := map[string]bool{}
	result := 0
	for design, ok := input.Line(); ok; design, ok = input.Line() {
		if aoc.possible(design, pats, memo) {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202419) countArrangements(design string, pats []string, memo map[string]int) int {
	if n, ok := memo[design]; ok {
		return n
	}
	n := 0
	for _, pat := range pats {
		if len(pat) > len(design) {
			continue
		}
		if pat == design[:len(pat)] {
			d := design[len(pat):]
			if d == "" {
				n++
			} else {
				n += aoc.countArrangements(d, pats, memo)
			}
		}
	}
	memo[design] = n
	return n
}

func (aoc *aoc202419) Part2(input *Input) string {
	pats := aoc.patterns(input)
	memo := map[string]int{}
	result := 0
	for design, ok := input.Line(); ok; design, ok = input.Line() {
		result += aoc.countArrangements(design, pats, memo)
	}
	return IntResult(result)
}
