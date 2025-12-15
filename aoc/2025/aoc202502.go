package main

func init() {
	Register(&aoc202502{
		AOC: AOC{
			Day: 2,
			Tests: []Test{
				Test{
					Input: `11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
1698522-1698528,446443-446449,38593856-38593862,565653-565659,
824824821-824824827,2121212118-2121212124
`,
					Part1: "1227775554",
					Part2: "4174379265",
				},
			},
		},
	})
}

type aoc202502 struct {
	AOC
}

func (aoc *aoc202502) nextRange(input *Input) (int, int, bool) {
	start, ok := input.Int()
	if !ok {
		return 0, 0, false
	}
	end, ok := input.Int()
	if !ok {
		return start, 0, false
	}
	return start, -end, true
}

func (aoc *aoc202502) countDigits(n int) int {
	count := 1
	for n >= 10 {
		count++
		n /= 10
	}
	return count
}

func (aoc *aoc202502) tenToThe(n int) int {
	result := 1
	for n > 0 {
		n--
		result *= 10
	}
	return result
}

func (aoc *aoc202502) rangeStart(i, repeatCount int) int {
	ndigits := aoc.countDigits(i)
	if ndigits%repeatCount != 0 {
		return aoc.tenToThe(ndigits / repeatCount)
	} else {
		return i / aoc.tenToThe(ndigits-ndigits/repeatCount)
	}
}

func (aoc *aoc202502) rangeEnd(i, repeatCount int) int {
	ndigits := aoc.countDigits(i)
	if ndigits%repeatCount != 0 {
		return aoc.tenToThe(ndigits/repeatCount) - 1
	} else {
		return i / aoc.tenToThe(ndigits-ndigits/repeatCount)
	}
}

func (aoc *aoc202502) repeat(n, count int) int {
	result := n
	shift := aoc.tenToThe(aoc.countDigits(n))
	for count > 1 {
		count--
		result = result*shift + n
	}
	return result
}

func (aoc *aoc202502) collectInvalids(start, end, repeatCount int, invalids map[int]bool) {
	seqMin := aoc.rangeStart(start, repeatCount)
	seqMax := aoc.rangeEnd(end, repeatCount)
	for seq := seqMin; seq <= seqMax; seq++ {
		i := aoc.repeat(seq, repeatCount)
		if i >= start && i <= end {
			invalids[i] = true
		}
	}
}

func (aoc *aoc202502) Part1(input *Input) string {
	result := 0
	for start, end, ok := aoc.nextRange(input); ok; start, end, ok = aoc.nextRange(input) {
		invalids := map[int]bool{}
		aoc.collectInvalids(start, end, 2, invalids)
		for i := range invalids {
			result += i
		}
	}
	return IntResult(result)
}

func (aoc *aoc202502) Part2(input *Input) string {
	result := 0
	for start, end, ok := aoc.nextRange(input); ok; start, end, ok = aoc.nextRange(input) {
		invalids := map[int]bool{}
		ndigits := aoc.countDigits(end)
		for repeatCount := 2; repeatCount <= ndigits; repeatCount++ {
			aoc.collectInvalids(start, end, repeatCount, invalids)
		}
		for i := range invalids {
			result += i
		}
	}
	return IntResult(result)
}
