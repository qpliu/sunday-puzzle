package main

func init() {
	Register(&aoc202008{
		AOC: AOC{
			Day:           8,
			InputFilename: "../2020/input/08.txt",
			Tests: []Test{
				Test{
					Input: `nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
`,
					Part1: "5",
					Part2: "8",
				},
			},
		},
	})
}

type aoc202008 struct {
	AOC
}

func (aoc *aoc202008) parse(input *Input) [][2]int {
	code := [][2]int{}
	for {
		op, ok := input.Word()
		if !ok {
			return code
		}
		arg, _ := input.Int()
		switch op {
		case "acc":
			code = append(code, [2]int{0, arg})
		case "jmp":
			code = append(code, [2]int{1, arg})
		case "nop":
			code = append(code, [2]int{2, arg})
		default:
			panic("bad input")
		}
	}
}

func (aoc *aoc202008) Part1(input *Input) string {
	code := aoc.parse(input)
	acc := 0
	ip := 0
	seen := map[int]bool{}
	for {
		if seen[ip] {
			return IntResult(acc)
		}
		seen[ip] = true
		switch code[ip][0] {
		case 0:
			acc += code[ip][1]
			ip++
		case 1:
			ip += code[ip][1]
		case 2:
			ip++
		default:
			panic("?")
		}
	}
}

func (aoc *aoc202008) Part2(input *Input) string {
	code := aoc.parse(input)

	try := func(ipchanged, acc, ip int) (int, bool) {
		seen := map[int]bool{ipchanged: true}
		for {
			if seen[ip] {
				return 0, false
			}
			if ip >= len(code) {
				return acc, true
			}
			seen[ip] = true
			switch code[ip][0] {
			case 0:
				acc += code[ip][1]
				ip++
			case 1:
				ip += code[ip][1]
			case 2:
				ip++
			default:
				panic("?")
			}
		}
	}

	acc := 0
	ip := 0
	for {
		switch code[ip][0] {
		case 0:
			acc += code[ip][1]
			ip++
		case 1:
			if result, ok := try(ip, acc, ip+1); ok {
				return IntResult(result)
			}
			ip += code[ip][1]
		case 2:
			if result, ok := try(ip, acc, ip+code[ip][1]); ok {
				return IntResult(result)
			}
			ip++
		default:
			panic("?")
		}
	}
}
