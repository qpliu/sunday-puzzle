package main

func init() {
	Register(&aoc202505{
		AOC: AOC{
			Day: 5,
			Tests: []Test{
				Test{
					Input: `3-5
10-14
16-20
12-18

1
5
8
11
17
32
`,
					Part1: "3",
					Part2: "14",
				},
			},
		},
	})
}

type aoc202505 struct {
	AOC
}

type aoc202505_list struct {
	lo, hi int
	next   *aoc202505_list
}

func (l *aoc202505_list) add(lo, hi int) *aoc202505_list {
	if l == nil {
		return &aoc202505_list{lo: lo, hi: hi}
	} else if hi+1 < l.lo {
		return &aoc202505_list{lo: lo, hi: hi, next: l}
	} else if l.hi+1 < lo {
		l.next = l.next.add(lo, hi)
		return l
	} else if hi <= l.hi {
		l.lo = min(lo, l.lo)
		return l
	} else {
		return l.next.add(min(lo, l.lo), max(hi, l.hi))
	}
}

func (aoc *aoc202505) parse(input *Input) *aoc202505_list {
	paragraph, _ := input.Paragraph()
	rangesInput := InputString(paragraph)
	var ranges *aoc202505_list
	for {
		lo, _ := rangesInput.Int()
		hi, ok := rangesInput.Int()
		if !ok {
			break
		}
		ranges = ranges.add(lo, -hi)
	}
	return ranges
}

func (aoc *aoc202505) Part1(input *Input) string {
	ranges := aoc.parse(input)
	count := 0
	for id, ok := input.Int(); ok; id, ok = input.Int() {
		for r := ranges; r != nil; r = r.next {
			if id < r.lo {
				break
			} else if id > r.hi {
				continue
			} else {
				count++
				break
			}
		}
	}
	return IntResult(count)
}

func (aoc *aoc202505) Part2(input *Input) string {
	count := 0
	for ranges := aoc.parse(input); ranges != nil; ranges = ranges.next {
		count += 1 + ranges.hi - ranges.lo
	}
	return IntResult(count)
}
