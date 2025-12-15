package main

import (
	"cmp"
	"slices"
)

func init() {
	Register(&aoc202508{
		AOC: AOC{
			Day: 8,
			Tests: []Test{
				Test{
					Input: `162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
`,
					Part1: "40",
					Part2: "25272",
				},
			},
		},
	})
}

type aoc202508 struct {
	AOC
}

type aoc202508_pair struct {
	aindex, bindex, xprod, dist2 int
}

type aoc202508_pairs []aoc202508_pair

func (pairs aoc202508_pairs) Len() int {
	return len(pairs)
}

func (pairs aoc202508_pairs) Less(i, j int) bool {
	return pairs[i].dist2 < pairs[j].dist2
}

func (pairs aoc202508_pairs) Swap(i, j int) {
	pairs[i], pairs[j] = pairs[j], pairs[i]
}

func (aoc *aoc202508) parse(input *Input) (int, aoc202508_pairs) {
	var coords [][3]int
	for {
		x, _ := input.Int()
		y, _ := input.Int()
		z, ok := input.Int()
		if !ok {
			break
		}
		coords = append(coords, [3]int{x, y, z})
	}
	nboxes := len(coords)
	var pairs aoc202508_pairs
	for aindex := range nboxes {
		ax := coords[aindex][0]
		ay := coords[aindex][1]
		az := coords[aindex][2]
		for bindex := range aindex {
			bx := coords[bindex][0]
			dx := coords[bindex][0] - ax
			dy := coords[bindex][1] - ay
			dz := coords[bindex][2] - az
			dist2 := dx*dx + dy*dy + dz*dz
			// heuristic distance cutoff
			// without the cutoff,
			// part 1 runs in about 100ms
			// and part 2 runs in about 85ms
			// on my computer with my input
			// with the cutoff,
			// both parts run in about 1.7ms
			// each
			if dist2 > 200000000 {
				continue
			}
			pairs = append(pairs, aoc202508_pair{
				aindex: aindex,
				bindex: bindex,
				xprod:  ax * bx,
				dist2:  dist2,
			})
		}
	}
	slices.SortFunc(pairs, func(a, b aoc202508_pair) int {
		return cmp.Compare(a.dist2, b.dist2)
	})
	return nboxes, pairs
}

type aoc202508_junctions struct {
	sizes, circuits []int
}

func (aoc *aoc202508) newJunctions(nboxes int) *aoc202508_junctions {
	sizes := make([]int, nboxes)
	circuits := make([]int, nboxes)
	for i := range nboxes {
		sizes[i] = 1
		circuits[i] = i
	}
	return &aoc202508_junctions{sizes, circuits}
}

func (junctions *aoc202508_junctions) connect(pair aoc202508_pair) (int, int) {
	ai := pair.aindex
	bi := pair.bindex
	ac := junctions.circuits[ai]
	bc := junctions.circuits[bi]
	acs := junctions.sizes[ac]
	bcs := junctions.sizes[bc]
	if ac == bc {
		return acs, pair.xprod
	} else if acs == 1 && bcs == 1 {
		junctions.circuits[bi] = ac
		junctions.sizes[ac] = 2
		junctions.sizes[bc] = 0
		return 2, pair.xprod
	} else if acs == 1 {
		junctions.circuits[ai] = bc
		junctions.sizes[bc] = bcs + 1
		junctions.sizes[ac] = 0
		return bcs + 1, pair.xprod
	} else if bcs == 1 {
		junctions.circuits[bi] = ac
		junctions.sizes[ac] = acs + 1
		junctions.sizes[bc] = 0
		return acs + 1, pair.xprod
	} else if acs > bcs {
		junctions.circuits[bi] = ac
		junctions.sizes[ac] = acs + bcs
		junctions.sizes[bc] = 0
		for i, ic := range junctions.circuits {
			if ic == bc {
				junctions.circuits[i] = ac
			}
		}
		return acs + bcs, pair.xprod
	} else {
		junctions.circuits[ai] = bc
		junctions.sizes[bc] = acs + bcs
		junctions.sizes[ac] = 0
		for i, ic := range junctions.circuits {
			if ic == ac {
				junctions.circuits[i] = bc
			}
		}
		return acs + bcs, pair.xprod
	}
}

func (aoc *aoc202508) part1(npairs int, input *Input) string {
	nboxes, pairs := aoc.parse(input)
	junctions := aoc.newJunctions(nboxes)
	for i, pair := range pairs {
		if i >= npairs {
			break
		}
		junctions.connect(pair)
	}
	size1 := 1
	size2 := 1
	size3 := 1
	for _, size := range junctions.sizes {
		if size > size1 {
			size1, size = size, size1
		}
		if size > size2 {
			size2, size = size, size2
		}
		if size > size3 {
			size3, size = size, size3
		}
	}
	return IntResult(size1 * size2 * size3)
}

func (aoc *aoc202508) Part1(input *Input) string {
	return aoc.part1(1000, input)
}

func (aoc *aoc202508) Test1(input *Input) string {
	return aoc.part1(10, input)
}

func (aoc *aoc202508) Part2(input *Input) string {
	nboxes, pairs := aoc.parse(input)
	junctions := aoc.newJunctions(nboxes)
	for _, pair := range pairs {
		size, xprod := junctions.connect(pair)
		if size == nboxes {
			return IntResult(xprod)
		}
	}
	panic("invalid input")
}
