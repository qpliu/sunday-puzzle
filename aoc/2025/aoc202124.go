package main

func init() {
	Register(&aoc202124{
		AOC: AOC{
			Day:           24,
			InputFilename: "../2021/input/24.txt",
			Tests: []Test{
				Test{
					Input: `
`,
					Part1: "",
					Part2: "",
				},
			},
		},
	})
}

type aoc202124 struct {
	AOC
}

func (aoc *aoc202124) alu(input *Input) Seq[[3]int] {
	expect := func(s string) {
		if word, ok := input.Word(); !ok || word != s {
			panic("bad input")
		}
	}
	return func(yield func([3]int) bool) {
		for range 14 {
			var alu [3]int
			expect("inp") // w = IN
			expect("w")
			expect("mul") // x = 0        -  x = top
			expect("x")
			expect("0")
			expect("add") // x = z
			expect("x")
			expect("z")
			expect("mod") // x = z mod 26
			expect("x")
			expect("26")
			expect("div") // z = z div A - if A = 26, pop
			expect("z")
			alu[0], _ = input.Int()
			expect("add") // x = x + B   - x = top + B
			expect("x")
			alu[1], _ = input.Int()
			expect("eql") // x = w eq x  - x = IN = top + B,
			expect("x")
			expect("w")
			expect("eql") // x = x eq 0  - x = IN /= top + B,
			expect("x")
			expect("0")
			expect("mul") // y = 0
			expect("y")
			expect("0")
			expect("add") // y = 25
			expect("y")
			expect("25")
			expect("mul") // y = 25*x
			expect("y")
			expect("x")
			expect("add") // y = 25*x+1
			expect("y")
			expect("1")
			expect("mul") // z = z*y     - if IN /= top + B,
			//                           -   always true when A = 1
			//                           -   must be made false
			//                           -   when A = 26
			//                           - then push IN+C
			expect("z")
			expect("y")
			expect("mul") // y = 0
			expect("y")
			expect("0")
			expect("add") // y = w
			expect("y")
			expect("w")
			expect("add") // y = y + C
			expect("y")
			alu[2], _ = input.Int()
			expect("mul") // y = y*x
			expect("y")
			expect("x")
			expect("add") // z = z + y
			expect("z")
			expect("y")
			if !yield(alu) {
				return
			}
		}
	}
}

func (aoc *aoc202124) result(part1 bool, input *Input) string {
	z := make([][2]int, 0, 14)
	digit := 0
	result := [14]byte{}
	for alu := range aoc.alu(input) {
		if alu[0] == 1 {
			z = append(z, [2]int{digit, alu[2]})
		} else {
			x := z[len(z)-1]
			z = z[:len(z)-1]
			b := alu[1]
			c := x[1]
			// result[digit] - b = result[x[0]] + c
			if part1 {
				// maximize result[x[0]]
				for r := 9; r >= 1; r-- {
					if r+b+c < 10 {
						result[x[0]] = '0' + byte(r)
						result[digit] = '0' + byte(r+b+c)
						break
					}
				}
			} else {
				// minimize result[x[0]]
				for r := 1; r < 10; r++ {
					if r+b+c > 0 {
						result[x[0]] = '0' + byte(r)
						result[digit] = '0' + byte(r+b+c)
						break
					}
				}
			}
		}
		digit++
	}
	return string(result[:])
}

func (aoc *aoc202124) Part1(input *Input) string {
	return aoc.result(true, input)
}

func (aoc *aoc202124) Part2(input *Input) string {
	return aoc.result(false, input)
}
