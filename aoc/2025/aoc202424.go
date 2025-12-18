package main

import (
	"fmt"
)

func init() {
	Register(&aoc202424{
		AOC: AOC{
			Day:           24,
			InputFilename: "../2024/input/24.txt",
			Tests: []Test{
				Test{
					Input: `x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
`,
					Part1: "4",
					Part2: "",
				},
				Test{
					Input: `x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
`,
					Part1: "2024",
					Part2: "",
				},
			},
		},
	})
}

type aoc202424 struct {
	AOC
}

func (aoc *aoc202424) parse(input *Input) (map[string]bool, map[string][3]string) {
	wires := map[string]bool{}
	paragraph, _ := input.Paragraph()
	in := InputString(paragraph)
	for {
		w, _ := in.Word()
		n, ok := in.Word()
		if !ok {
			break
		}
		wires[w[:len(w)-1]] = n == "1"
	}
	gates := map[string][3]string{}
	for {
		a, _ := input.Word()
		op, _ := input.Word()
		b, _ := input.Word()
		input.Word()
		w, ok := input.Word()
		if !ok {
			break
		}
		gates[w] = [3]string{a, op, b}
	}
	return wires, gates
}

func (aoc *aoc202424) wire(wires map[string]bool, gates map[string][3]string, wire string) bool {
	if v, ok := wires[wire]; ok {
		return v
	}
	gate := gates[wire]
	a := aoc.wire(wires, gates, gate[0])
	b := aoc.wire(wires, gates, gate[2])
	w := false
	switch gate[1] {
	case "AND":
		w = a && b
	case "OR":
		w = a || b
	case "XOR":
		w = a != b
	}
	wires[wire] = w
	return w
}

func (aoc *aoc202424) Part1(input *Input) string {
	wires, gates := aoc.parse(input)
	result := 0
	bit := 1
	for z := 0; ; z++ {
		wire := fmt.Sprintf("z%02d", z)
		if _, ok := gates[wire]; !ok {
			break
		}
		if aoc.wire(wires, gates, wire) {
			result |= bit
		}
		bit <<= 1
	}
	return IntResult(result)
}

func (aoc *aoc202424) Part2(input *Input) string {
	return IntResult(0)
}
