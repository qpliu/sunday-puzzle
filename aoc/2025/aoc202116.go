package main

import (
	"math"
)

func init() {
	Register(&aoc202116{
		AOC: AOC{
			Day:           16,
			InputFilename: "../2021/input/16.txt",
			Tests: []Test{
				Test{
					Input: `8A004A801A8002F478`,
					Part1: "16",
					Part2: "",
				},
				Test{
					Input: `620080001611562C8802118E34`,
					Part1: "12",
					Part2: "",
				},
				Test{
					Input: `C0015000016115A2E0802F182340`,
					Part1: "23",
					Part2: "",
				},
				Test{
					Input: `A0016C880162017C3686B18A3D4780`,
					Part1: "31",
					Part2: "",
				},
				Test{
					Input: `C200B40A82`,
					Part1: "",
					Part2: "3",
				},
				Test{
					Input: `04005AC33890`,
					Part1: "",
					Part2: "54",
				},
				Test{
					Input: `880086C3E88112`,
					Part1: "",
					Part2: "7",
				},
				Test{
					Input: `CE00C43D881120`,
					Part1: "",
					Part2: "9",
				},
				Test{
					Input: `D8005AC2A8F0`,
					Part1: "",
					Part2: "1",
				},
				Test{
					Input: `F600BC2D8F`,
					Part1: "",
					Part2: "0",
				},
				Test{
					Input: `9C005AC2F8F0`,
					Part1: "",
					Part2: "0",
				},
				Test{
					Input: `9C0141080250320F1802104A08`,
					Part1: "",
					Part2: "1",
				},
			},
		},
	})
}

type aoc202116 struct {
	AOC
}

func (aoc *aoc202116) bits(input *Input) []bool {
	bits := []bool{}
	for ch := range input.Chars() {
		if ch >= 'A' {
			ch -= 'A' - 10
		} else if ch >= '0' {
			ch -= '0'
		} else {
			return bits
		}
		for bit := byte(8); bit > 0; bit >>= 1 {
			bits = append(bits, bit&ch != 0)
		}
	}
	return bits
}

func (aoc *aoc202116) readInt(bits []bool, length int) (int, []bool, bool) {
	i := 0
	for range length {
		if len(bits) == 0 {
			return 0, nil, false
		}
		i <<= 1
		if bits[0] {
			i |= 1
		}
		bits = bits[1:]
	}
	return i, bits, true
}

func (aoc *aoc202116) parse1(bits []bool) (int, []bool, bool) {
	versions, bits, ok := aoc.readInt(bits, 3)
	if !ok {
		return 0, nil, false
	}
	typeID, bits, _ := aoc.readInt(bits, 3)
	switch typeID {
	case 4:
		for {
			val, bits2, ok := aoc.readInt(bits, 5)
			if !ok {
				return 0, nil, false
			}
			bits = bits2
			if val&16 == 0 {
				break
			}
		}
	default:
		lengthTypeID, bits2, ok := aoc.readInt(bits, 1)
		if !ok {
			return 0, nil, false
		}
		bits = bits2
		switch lengthTypeID {
		case 0:
			length, bits2, _ := aoc.readInt(bits, 15)
			bits = bits2[length:]
			bits2 = bits2[:length]
			for {
				v, bits3, ok := aoc.parse1(bits2)
				bits2 = bits3
				if !ok {
					break
				}
				versions += v
			}
		case 1:
			count, bits2, _ := aoc.readInt(bits, 11)
			bits = bits2
			for range count {
				v, bits2, _ := aoc.parse1(bits)
				bits = bits2
				versions += v
			}
		}
	}
	return versions, bits, true
}

func (aoc *aoc202116) Part1(input *Input) string {
	result, _, ok := aoc.parse1(aoc.bits(input))
	if !ok {
		panic("bad input")
	}
	return IntResult(result)
}

func (aoc *aoc202116) parse2(bits []bool) (int, []bool, bool) {
	_, bits, ok := aoc.readInt(bits, 3)
	if !ok {
		return 0, nil, false
	}
	typeID, bits, _ := aoc.readInt(bits, 3)
	switch typeID {
	case 4:
		v := 0
		for {
			val, bits2, ok := aoc.readInt(bits, 5)
			if !ok {
				return 0, nil, false
			}
			v <<= 4
			v |= val & 15
			bits = bits2
			if val&16 == 0 {
				return v, bits, true
			}
		}
	default:
		lengthTypeID, bits2, ok := aoc.readInt(bits, 1)
		if !ok {
			return 0, nil, false
		}
		bits = bits2
		operands := []int{}
		switch lengthTypeID {
		case 0:
			length, bits2, _ := aoc.readInt(bits, 15)
			bits = bits2[length:]
			bits2 = bits2[:length]
			for {
				val, bits3, ok := aoc.parse2(bits2)
				bits2 = bits3
				if !ok {
					break
				}
				operands = append(operands, val)
			}
		case 1:
			count, bits2, _ := aoc.readInt(bits, 11)
			bits = bits2
			for range count {
				val, bits2, _ := aoc.parse2(bits)
				bits = bits2
				operands = append(operands, val)
			}
		}
		var result int
		switch typeID {
		case 0:
			result = 0
			for _, val := range operands {
				result += val
			}
		case 1:
			result = 1
			for _, val := range operands {
				result *= val
			}
		case 2:
			result = math.MaxInt
			for _, val := range operands {
				result = min(result, val)
			}
		case 3:
			result = math.MinInt
			for _, val := range operands {
				result = max(result, val)
			}
		case 5:
			if operands[0] > operands[1] {
				result = 1
			}
		case 6:
			if operands[0] < operands[1] {
				result = 1
			}
		case 7:
			if operands[0] == operands[1] {
				result = 1
			}
		}
		return result, bits, true
	}
}

func (aoc *aoc202116) Part2(input *Input) string {
	result, _, ok := aoc.parse2(aoc.bits(input))
	if !ok {
		panic("bad input")
	}
	return IntResult(result)
}
