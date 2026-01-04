package main

import (
	"math/big"
)

func init() {
	Register(&aoc202221{
		AOC: AOC{
			Day:           21,
			InputFilename: "../2022/input/21.txt",
			Tests: []Test{
				Test{
					Input: `root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
`,
					Part1: "152",
					Part2: "301",
				},
			},
		},
	})
}

type aoc202221 struct {
	AOC
}

func (aoc *aoc202221) parse(input *Input) map[string][]string {
	jobs := map[string][]string{}
	for line := range input.Lines() {
		in := InputString(line)
		name, _ := in.Word()
		name = name[:len(name)-1]
		for w := range in.Words() {
			jobs[name] = append(jobs[name], w)
		}
	}
	return jobs
}

func (aoc *aoc202221) Part1(input *Input) string {
	jobs := aoc.parse(input)
	numbers := map[string]int{}

	var job func(string) int
	job = func(name string) int {
		if n, ok := numbers[name]; ok {
			return n
		}
		expr := jobs[name]
		if len(expr) == 1 {
			n, _ := InputString(expr[0]).Int()
			numbers[name] = n
			return n
		}
		a := job(expr[0])
		b := job(expr[2])
		switch expr[1] {
		case "+":
			numbers[name] = a + b
			return a + b
		case "-":
			numbers[name] = a - b
			return a - b
		case "*":
			numbers[name] = a * b
			return a * b
		case "/":
			numbers[name] = a / b
			return a / b
		default:
			panic("bad input")
		}
	}

	return IntResult(job("root"))
}

func (aoc *aoc202221) Part2(input *Input) string {
	jobs := aoc.parse(input)
	numbers := map[string][3]*big.Rat{}

	var job func(string) [3]*big.Rat
	job = func(name string) [3]*big.Rat {
		if name == "humn" {
			return [3]*big.Rat{
				&big.Rat{},
				big.NewRat(1, 1),
				&big.Rat{},
			}
		}
		if n, ok := numbers[name]; ok {
			return n
		}
		expr := jobs[name]
		if len(expr) == 1 {
			n, _ := InputString(expr[0]).Int()
			r := [3]*big.Rat{big.NewRat(int64(n), 1), &big.Rat{}, &big.Rat{}}
			numbers[name] = r
			return r
		}
		a := job(expr[0])
		b := job(expr[2])
		switch expr[1] {
		case "+":
			r := [3]*big.Rat{(&big.Rat{}).Add(a[0], b[0]), (&big.Rat{}).Add(a[1], b[1]), (&big.Rat{}).Add(a[2], b[2])}
			numbers[name] = r
			return r
		case "-":
			r := [3]*big.Rat{(&big.Rat{}).Sub(a[0], b[0]), (&big.Rat{}).Sub(a[1], b[1]), (&big.Rat{}).Sub(a[2], b[2])}
			numbers[name] = r
			return r
		case "*":
			// assume only one of a[1], a[2], b[1], b[2] can be nonzero
			r := [3]*big.Rat{
				(&big.Rat{}).Mul(a[0], b[0]),
				(&big.Rat{}).Add((&big.Rat{}).Mul(a[0], b[1]), (&big.Rat{}).Mul(a[1], b[0])),
				(&big.Rat{}).Add((&big.Rat{}).Mul(a[0], b[2]), (&big.Rat{}).Mul(a[2], b[0])),
			}
			numbers[name] = r
			return r
		case "/":
			// assume only one of a[1], a[2], b[1], b[2] can be nonzero
			if b[1].Sign() == 0 && b[2].Sign() == 0 {
				r := [3]*big.Rat{
					(&big.Rat{}).Quo(a[0], b[0]),
					(&big.Rat{}).Quo(a[1], b[0]),
					(&big.Rat{}).Quo(a[2], b[0]),
				}
				numbers[name] = r
				return r
			}
			panic("case not handled")
		default:
			panic("bad input")
		}
	}

	root := jobs["root"]
	a := job(root[0])
	b := job(root[2])

	if a[1].Sign() != 0 {
		return b[0].Quo(b[0].Sub(b[0], a[0]), a[1]).RatString()
	}
	if a[2].Sign() != 0 {
		return a[2].Quo(a[2], b[0].Sub(b[0], a[0])).RatString()
	}
	if b[1].Sign() != 0 {
		return a[0].Quo(a[0].Sub(a[0], b[0]), b[1]).RatString()
	}
	if b[2].Sign() != 0 {
		return b[2].Quo(b[2], a[0].Sub(a[0], b[0])).RatString()
	}
	return "FAIL"
}
