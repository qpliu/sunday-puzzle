package main

func init() {
	Register(&aoc202007{
		AOC: AOC{
			Day:           7,
			InputFilename: "../2020/input/07.txt",
			Tests: []Test{
				Test{
					Input: `light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
`,
					Part1: "4",
					Part2: "32",
				},
				Test{
					Input: `shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
`,
					Part1: "",
					Part2: "126",
				},
			},
		},
	})
}

type aoc202007 struct {
	AOC
}

func (aoc *aoc202007) parse(input *Input) map[[2]string]map[[2]string]int {
	bags := map[[2]string]map[[2]string]int{}
	for line := range input.Lines() {
		bag := map[[2]string]int{}
		in := InputString(line)
		b1, _ := in.Word()
		b2, _ := in.Word()
		bagId := [2]string{b1, b2}
		for {
			n, ok := in.Int()
			if !ok {
				break
			}
			b1, _ := in.Word()
			b2, _ := in.Word()
			bag[[2]string{b1, b2}] = n
		}
		bags[bagId] = bag
	}
	return bags
}

func (aoc *aoc202007) Part1(input *Input) string {
	bags := aoc.parse(input)
	memo := map[[2]string]bool{}
	var check func([2]string) bool
	check = func(id [2]string) bool {
		if can, ok := memo[id]; ok {
			return can
		}
		for containedID := range bags[id] {
			if containedID == [2]string{"shiny", "gold"} || check(containedID) {
				memo[id] = true
				return true
			}
		}
		memo[id] = false
		return false
	}
	result := 0
	for id := range bags {
		if check(id) {
			result++
		}
	}
	return IntResult(result)
}

func (aoc *aoc202007) Part2(input *Input) string {
	bags := aoc.parse(input)
	memo := map[[2]string]int{}
	var count func([2]string) int
	count = func(id [2]string) int {
		if n, ok := memo[id]; ok {
			return n
		}
		total := 1
		for containedID, n := range bags[id] {
			total += n * count(containedID)
		}
		memo[id] = total
		return total
	}
	return IntResult(count([2]string{"shiny", "gold"}) - 1)
}
