package main

func init() {
	Register(&aoc202206{
		AOC: AOC{
			Day:           6,
			InputFilename: "../2022/input/06.txt",
			Tests: []Test{
				Test{
					Input: `mjqjpqmgbljsphdztnvjfqwrcgsmlb
`,
					Part1: "7",
					Part2: "19",
				},
				Test{
					Input: `bvwbjplbgvbhsrlpgdmjqwftvncz
`,
					Part1: "5",
					Part2: "23",
				},
				Test{
					Input: `nppdvjthqldpwncqszvftbrmjlhg
`,
					Part1: "6",
					Part2: "23",
				},
				Test{
					Input: `nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
`,
					Part1: "10",
					Part2: "29",
				},
				Test{
					Input: `zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
`,
					Part1: "11",
					Part2: "26",
				},
			},
		},
	})
}

type aoc202206 struct {
	AOC
}

func (aoc *aoc202206) result(n int, input *Input) string {
	buffer := make([]byte, n)
	counts := map[byte]int{}
	for i := range n {
		ch, _ := input.Char()
		buffer[i] = ch
		counts[ch]++
	}
	i := n
	for {
		if len(counts) == n {
			return IntResult(i)
		}
		ch, ok := input.Char()
		if !ok {
			panic("bad input")
		}
		i++
		if counts[buffer[0]] > 1 {
			counts[buffer[0]]--
		} else {
			delete(counts, buffer[0])
		}
		copy(buffer, buffer[1:])
		buffer[n-1] = ch
		counts[ch]++
	}
}

func (aoc *aoc202206) Part1(input *Input) string {
	return aoc.result(4, input)
}

func (aoc *aoc202206) Part2(input *Input) string {
	return aoc.result(14, input)
}
