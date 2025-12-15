package main

func init() {
	Register(&aoc202511{
		AOC: AOC{
			Day: 11,
			Tests: []Test{
				Test{
					Input: `aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out
`,
					Part1: "5",
					Part2: "",
				},
				Test{
					Input: `svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out
`,
					Part1: "",
					Part2: "2",
				},
			},
		},
	})
}

type aoc202511 struct {
	AOC
	out, svr, you, fft, dac int
}

func (aoc *aoc202511) encode(dev string) int {
	return int(dev[0]) + int(dev[1])<<8 + int(dev[2])<<16
}

func (aoc *aoc202511) Reset() {
	aoc.out = aoc.encode("out")
	aoc.svr = aoc.encode("svr")
	aoc.you = aoc.encode("you")
	aoc.fft = aoc.encode("fft")
	aoc.dac = aoc.encode("dac")
}

func (aoc *aoc202511) parse(input *Input) map[int][]int {
	rack := map[int][]int{}
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		linput := InputString(line)
		word, _ := linput.Word()
		dev := aoc.encode(word)
		var devs []int
		for word, ok := linput.Word(); ok; word, ok = linput.Word() {
			devs = append(devs, aoc.encode(word))
		}
		rack[dev] = devs
	}
	return rack
}

func (aoc *aoc202511) paths1(rack map[int][]int, dev int, memo map[int]int) int {
	if dev == aoc.out {
		return 1
	}
	count, ok := memo[dev]
	if ok {
		return count
	}
	for _, d := range rack[dev] {
		count += aoc.paths1(rack, d, memo)
	}
	memo[dev] = count
	return count
}

func (aoc *aoc202511) Part1(input *Input) string {
	return IntResult(aoc.paths1(aoc.parse(input), aoc.you, make(map[int]int)))
}

func (aoc *aoc202511) paths2(rack map[int][]int, dev [2]int, memo map[[2]int]int) int {
	if dev[0] == aoc.out {
		if dev[1] == 3 {
			return 1
		} else {
			return 0
		}
	}
	count, ok := memo[dev]
	if ok {
		return count
	}
	if dev[0] == aoc.dac {
		for _, d := range rack[dev[0]] {
			count += aoc.paths2(rack, [2]int{d, dev[1] | 1}, memo)
		}
	} else if dev[0] == aoc.fft {
		for _, d := range rack[dev[0]] {
			count += aoc.paths2(rack, [2]int{d, dev[1] | 2}, memo)
		}
	} else {
		for _, d := range rack[dev[0]] {
			count += aoc.paths2(rack, [2]int{d, dev[1]}, memo)
		}
	}
	memo[dev] = count
	return count
}

func (aoc *aoc202511) Part2(input *Input) string {
	return IntResult(aoc.paths2(aoc.parse(input), [2]int{aoc.svr, 0}, map[[2]int]int{}))
}
