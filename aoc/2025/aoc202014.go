package main

func init() {
	Register(&aoc202014{
		AOC: AOC{
			Day:           14,
			InputFilename: "../2020/input/14.txt",
			Tests: []Test{
				Test{
					Input: `mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
`,
					Part1: "165",
					Part2: "",
				},
				Test{
					Input: `mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
`,
					Part1: "",
					Part2: "208",
				},
			},
		},
	})
}

type aoc202014 struct {
	AOC
}

func (aoc *aoc202014) Part1(input *Input) string {
	mask0, mask1 := 0, 0
	mask := func() {
		input.Skip("\n")
		if !input.Skip("mask = ") {
			return
		}
		mask0, mask1 = 0, 0
		for i := range 36 {
			ch, _ := input.Char()
			switch ch {
			case 'X':
			case '0':
				mask0 |= 1 << (35 - i)
			case '1':
				mask1 |= 1 << (35 - i)
			}
		}
	}
	mem := map[int]int{}
	for {
		mask()
		loc, ok := input.Int()
		if !ok {
			break
		}
		val, _ := input.Int()
		mem[loc] = (val &^ mask0) | mask1
	}
	result := 0
	for _, val := range mem {
		result += val
	}
	return IntResult(result)
}

func (aoc *aoc202014) Part2(input *Input) string {
	mask1, maskX := 0, 0
	xsize := 1
	mask := func() {
		input.Skip("\n")
		if !input.Skip("mask = ") {
			return
		}
		mask1, maskX = 0, 0
		xsize = 1
		for i := range 36 {
			ch, _ := input.Char()
			switch ch {
			case 'X':
				maskX |= 1 << (35 - i)
				xsize <<= 1
			case '0':
			case '1':
				mask1 |= 1 << (35 - i)
			}
		}
	}
	mem := map[int]int{}
	for {
		mask()
		loc, ok := input.Int()
		if !ok {
			break
		}
		val, _ := input.Int()
		for x := range xsize {
			mloc := (loc &^ maskX) | mask1
			for bit := range 36 {
				if maskX&(1<<bit) != 0 {
					if x&1 != 0 {
						mloc |= (1 << bit)
					}
					x >>= 1
				}
			}
			mem[mloc] = val
		}
	}
	result := 0
	for _, val := range mem {
		result += val
	}
	return IntResult(result)
}
