package main

import (
	"math"
)

func init() {
	Register(&aoc202421{
		AOC: AOC{
			Day:           21,
			InputFilename: "../2024/input/21.txt",
			Tests: []Test{
				Test{
					Input: `029A
980A
179A
456A
379A
`,
					Part1: "126384",
					Part2: "",
				},
			},
		},
	})
}

type aoc202421 struct {
	AOC
}

func (aoc *aoc202421) makeTable(nchain int) map[string]int {
	npadMoves := map[string][]string{
		"A0": {"A<A"},
		"A1": {"A^<<A"},
		"A2": {"A<^A", "A^<A"},
		"A3": {"A^A"},
		"A4": {"A^^<<A"},
		"A5": {"A<^^A", "A^^<A"},
		"A6": {"A^^A"},
		"A7": {"A^^^<<A"},
		"A8": {"A<^^^A", "A^^^<A"},
		"A9": {"A^^^A"},
		"AA": {"AA"},
		"00": {"AA"},
		"01": {"A^<A"},
		"02": {"A^A"},
		"03": {"A^>A"},
		"04": {"A^^<A"},
		"05": {"A^^A"},
		"06": {"A^^>A", "A>^^A"},
		"07": {"A^^^<A"},
		"08": {"A^^^A"},
		"09": {"A^^^>A", "A>^^^A"},
		"0A": {"A>A"},
		"10": {"A>vA"},
		"11": {"AA"},
		"12": {"A>A"},
		"13": {"A>>A"},
		"14": {"A^A"},
		"15": {"A^>A", "A>^A"},
		"16": {"A^>>A", "A>>^A"},
		"17": {"A^^A"},
		"18": {"A^^>A", "A>^^A"},
		"19": {"A^^>>A", "A>>^^A"},
		"1A": {"A>>vA"},
		"20": {"AvA"},
		"21": {"A<A"},
		"22": {"AA"},
		"23": {"A>A"},
		"24": {"A<^A", "A^<A"},
		"25": {"A^A"},
		"26": {"A^>A", "A>^A"},
		"27": {"A<^^A", "A^^<A"},
		"28": {"A^^A"},
		"29": {"A^^>A", "A>^^A"},
		"2A": {"A>vA", "Av>A"},
		"30": {"A<vA", "Av<A"},
		"31": {"A<<A"},
		"32": {"A<A"},
		"33": {"AA"},
		"34": {"A^<<A", "A<<^A"},
		"35": {"A^<A", "A<^A"},
		"36": {"A^A"},
		"37": {"A^^<<A", "A<<^^A"},
		"38": {"A^^<A", "A<^^A"},
		"39": {"A^^A"},
		"3A": {"AvA"},
		"40": {"A>vvA"},
		"41": {"AvA"},
		"42": {"A>vA", "Av>A"},
		"43": {"A>>vA", "Av>>A"},
		"44": {"AA"},
		"45": {"A>A"},
		"46": {"A>>A"},
		"47": {"A^A"},
		"48": {"A^>A", "A>^A"},
		"49": {"A^>>A", "A>>^A"},
		"4A": {"A>>vvA"},
		"50": {"AvvA"},
		"51": {"A<vA", "Av<A"},
		"52": {"AvA"},
		"53": {"A>vA", "Av>A"},
		"54": {"A<A"},
		"55": {"AA"},
		"56": {"A>A"},
		"57": {"A^<A", "A<^A"},
		"58": {"A^A"},
		"59": {"A^>A", "A>^A"},
		"5A": {"A>vvA", "Avv>A"},
		"60": {"A<vvA", "Avv<A"},
		"61": {"A<<vA", "Av<<A"},
		"62": {"A<vA", "Av<A"},
		"63": {"AvA"},
		"64": {"A<<A"},
		"65": {"A<A"},
		"66": {"AA"},
		"67": {"A^<<A", "A<<^A"},
		"68": {"A^<A", "A<^A"},
		"69": {"A^A"},
		"6A": {"AvvA"},
		"70": {"A>vvvA"},
		"71": {"AvvA"},
		"72": {"A>vvA", "Avv>A"},
		"73": {"A>>vvA", "Avv>>A"},
		"74": {"AvA"},
		"75": {"A>vA", "Av>A"},
		"76": {"A>>vA", "Av>>A"},
		"77": {"AA"},
		"78": {"A>A"},
		"79": {"A>>A"},
		"7A": {"A>>vvvA"},
		"80": {"AvvvA"},
		"81": {"A<vvA", "Avv<A"},
		"82": {"AvvA"},
		"83": {"A>vvA", "Avv>A"},
		"84": {"A<vA", "Av<A"},
		"85": {"AvA"},
		"86": {"Av>A", "A>vA"},
		"87": {"A<A"},
		"88": {"AA"},
		"89": {"A>A"},
		"8A": {"A>vvvA", "Avvv>A"},
		"90": {"A<vvvA", "Avvv<A"},
		"91": {"A<<vvA", "Avv<<A"},
		"92": {"A<vvA", "Avv<A"},
		"93": {"AvvA"},
		"94": {"A<<vA", "Av<<A"},
		"95": {"A<vA", "Av<A"},
		"96": {"AvA"},
		"97": {"A<<A"},
		"98": {"A<A"},
		"99": {"AA"},
		"9A": {"AvvvA"},
	}
	dpadMoves := map[string][]string{
		"^^": {"AA"},
		"^A": {"A>A"},
		"^<": {"Av<A"},
		"^v": {"AvA"},
		"^>": {"A>vA", "Av>A"},
		"A^": {"A<A"},
		"AA": {"AA"},
		"A<": {"Av<<A"},
		"Av": {"A<vA", "Av<A"},
		"A>": {"AvA"},
		"<^": {"A>^A"},
		"<A": {"A>>^A"},
		"<<": {"AA"},
		"<v": {"A>A"},
		"<>": {"A>>A"},
		"v^": {"A^A"},
		"vA": {"A^>A", "A>^A"},
		"v<": {"A<A"},
		"vv": {"AA"},
		"v>": {"A>A"},
		">^": {"A^<A", "A<^A"},
		">A": {"A^A"},
		"><": {"A<<A"},
		">v": {"A<A"},
		">>": {"AA"},
	}
	dpadPresses := map[string]int{}
	for k, v := range dpadMoves {
		dpadPresses[k] = len(v[0]) - 1
	}
	for range nchain - 1 {
		nextPresses := map[string]int{}
		for k, moves := range dpadMoves {
			minPresses := math.MaxInt
			for _, seq := range moves {
				presses := 0
				for i := range len(seq) - 1 {
					presses += dpadPresses[seq[i:i+2]]
				}
				minPresses = min(minPresses, presses)
			}
			nextPresses[k] = minPresses
		}
		dpadPresses = nextPresses
	}

	npadPresses := map[string]int{}
	for k, moves := range npadMoves {
		minPresses := math.MaxInt
		for _, seq := range moves {
			presses := 0
			for i := range len(seq) - 1 {
				presses += dpadPresses[seq[i:i+2]]
			}
			minPresses = min(minPresses, presses)
		}
		npadPresses[k] = minPresses
	}
	return npadPresses
}

func (aoc *aoc202421) run(nchain int, input *Input) int {
	result := 0
	table := aoc.makeTable(nchain)
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		presses := 0
		code := "A" + line
		for i := range line {
			presses += table[code[i:i+2]]
		}
		codeNum, _ := InputString(line).Int()
		result += presses * codeNum
	}
	return result
}

func (aoc *aoc202421) Part1(input *Input) string {
	return IntResult(aoc.run(2, input))
}

func (aoc *aoc202421) Part2(input *Input) string {
	return IntResult(aoc.run(25, input))
}
