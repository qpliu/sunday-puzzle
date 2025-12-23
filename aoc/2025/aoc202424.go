package main

import (
	"bytes"
	"fmt"
	"slices"
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
		wire, ok := input.Word()
		if !ok {
			break
		}
		gates[wire] = [3]string{min(a, b), op, max(a, b)}
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
	v := false
	switch gate[1] {
	case "AND":
		v = a && b
	case "OR":
		v = a || b
	case "XOR":
		v = a != b
	}
	wires[wire] = v
	return v
}

func (aoc *aoc202424) Part1(input *Input) string {
	wires, gates := aoc.parse(input)
	result := 0
	bit := 1
	for z := range len(gates) {
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
	wires, gates := aoc.parse(input)
	nbits := len(wires) / 2

	// in my input, x and y only appear as X AND Y and X XOR Y
	ands := make([]string, nbits)
	xors := make([]string, nbits)
	andIndex := map[string]int{}
	xorIndex := map[string]int{}
	for k, v := range gates {
		if v[0][0] == 'x' && v[2][0] == 'y' {
			bit, _ := InputString(v[0][1:]).Int()
			if v[1] == "AND" {
				ands[bit] = k
				andIndex[k] = bit
			} else if v[1] == "XOR" {
				xors[bit] = k
				xorIndex[k] = bit
			}
		}
	}

	swaps := map[string]bool{}
	if "z00" != xors[0] {
		swaps["z00"] = true
		swaps[xors[0]] = true
	}

	// z = carryIn xor (x xor y)
	// carryOut = (x and y) or (carryIn and (x xor y))

	carryIns := make([]string, nbits)
	for k, v := range gates {
		if v[1] != "XOR" {
			continue
		}
		if _, ok := xorIndex[k]; ok {
			continue
		}
		carryIn := v[2]
		i, ok := xorIndex[v[0]]
		if !ok {
			carryIn = v[0]
			i, ok = xorIndex[v[2]]
			if !ok {
				continue
			}
		}
		carryIns[i] = carryIn
		z := fmt.Sprintf("z%02d", i)
		if z != k {
			swaps[z] = true
			swaps[k] = true
		}
	}
	if carryIns[1] != ands[0] {
		swaps[carryIns[1]] = true
		swaps[ands[0]] = true
	}

	carryAnds := make([]string, nbits)
	for k, v := range gates {
		if v[1] != "AND" {
			continue
		}
		if _, ok := andIndex[k]; ok {
			continue
		}
		carryIn := v[2]
		i, ok := xorIndex[v[0]]
		if !ok {
			carryIn = v[0]
			i, ok = xorIndex[v[2]]
			if !ok {
				continue
			}
		}
		carryAnds[i] = k
		if carryIns[i] == "" {
			panic("bad data")
		} else if carryIns[i] != carryIn {
			swaps[carryIns[i]] = true
			swaps[carryIn] = true
		}
	}

	carryOuts := make([]string, nbits)
	carryOuts[0] = ands[0]
	for k, v := range gates {
		if v[1] != "OR" {
			continue
		}
		carryAnd := v[2]
		i, ok := andIndex[v[0]]
		if !ok {
			carryAnd = v[0]
			i, ok = andIndex[v[2]]
			if !ok {
				continue
			}
		}
		carryOuts[i] = k
		if carryAnds[i] == "" {
			panic("bad data")
		} else if carryAnds[i] != carryAnd {
			swaps[carryAnds[i]] = true
			swaps[carryAnd] = true
		}
	}
	zlast := fmt.Sprintf("z%02d", nbits)
	if carryOuts[nbits-1] != zlast {
		swaps[zlast] = true
		swaps[carryOuts[nbits-1]] = true
	}

	for i := range nbits {
		if i == 0 || carryIns[i] != "" || carryOuts[i-1] == "" {
			continue
		}
		carryIn := carryOuts[i-1]
		for _, v := range gates {
			j := -1
			if v[0] == carryIn {
				j = 2
			} else if v[2] == carryIn {
				j = 0
			}
			if j < 0 {
				continue
			}
			swaps[xors[i]] = true
			swaps[v[j]] = true
			break
		}
	}

	s := []string{}
	for k := range swaps {
		s = append(s, k)
	}
	slices.Sort(s)
	buf := &bytes.Buffer{}
	for _, swap := range s {
		buf.WriteByte(',')
		buf.WriteString(swap)
	}
	buf.ReadByte()
	return buf.String()
}
