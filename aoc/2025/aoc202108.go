package main

import (
	"math/bits"
	"runtime"
)

func init() {
	Register(&aoc202108{
		AOC: AOC{
			Day:           8,
			InputFilename: "../2021/input/08.txt",
			Tests: []Test{
				Test{
					Input: `acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
`,
					Part1: "",
					Part2: "5353",
				},
				Test{
					Input: `be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
`,
					Part1: "26",
					Part2: "61229",
				},
			},
		},
	})
}

type aoc202108 struct {
	AOC
}

func (aoc *aoc202108) parse(input *Input) Seq[[14]byte] {
	toBits := func(signal string) byte {
		bits := byte(0)
		for _, b := range signal {
			bits |= 1 << (b - 'a')
		}
		return bits
	}
	return func(yield func([14]byte) bool) {
		for {
			entry := [14]byte{}
			for i := range 10 {
				word, ok := input.Word()
				if !ok {
					return
				}
				entry[i] = toBits(word)
			}
			input.Word()
			for i := range 4 {
				word, ok := input.Word()
				if !ok {
					return
				}
				entry[10+i] = toBits(word)
			}
			if !yield(entry) {
				return
			}
		}
	}
}

func (aoc *aoc202108) Part1(input *Input) string {
	in := make(chan [14]byte)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for entry := range in {
				for _, b := range entry[10:14] {
					switch bits.OnesCount8(b) {
					case 2, 3, 4, 7:
						result++
					}
				}
			}
			out <- result
		}()
	}
	for entry := range aoc.parse(input) {
		in <- entry
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202108) decode(entry [14]byte) int {
	var one, seven, four, eight byte
	for _, b := range entry[:10] {
		switch bits.OnesCount8(b) {
		case 2:
			one = b
		case 3:
			seven = b
		case 4:
			four = b
		case 7:
			eight = b
		}
	}
	var two, three, five byte
	var zero, six, nine byte
	for _, b := range entry[:10] {
		switch bits.OnesCount8(b) {
		case 5:
			if one&b == one {
				three = b
			} else if bits.OnesCount8(four&b) == 2 {
				two = b
			} else {
				five = b
			}
		case 6:
			if four&b == four {
				nine = b
			} else if one&b == one {
				zero = b
			} else {
				six = b
			}
		}
	}
	digit := func(b byte) int {
		if b == zero {
			return 0
		} else if b == one {
			return 1
		} else if b == two {
			return 2
		} else if b == three {
			return 3
		} else if b == four {
			return 4
		} else if b == five {
			return 5
		} else if b == six {
			return 6
		} else if b == seven {
			return 7
		} else if b == eight {
			return 8
		} else if b == nine {
			return 9
		} else {
			panic("?")
		}
	}
	return 1000*digit(entry[10]) + 100*digit(entry[11]) + 10*digit(entry[12]) + digit(entry[13])
}

func (aoc *aoc202108) Part2(input *Input) string {
	in := make(chan [14]byte)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			for entry := range in {
				result += aoc.decode(entry)
			}
			out <- result
		}()
	}
	for entry := range aoc.parse(input) {
		in <- entry
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}
