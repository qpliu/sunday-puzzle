package main

import (
	"math"
	"slices"
)

func init() {
	Register(&aoc202305{
		AOC: AOC{
			Day:           5,
			InputFilename: "../2023/input/05.txt",
			Tests: []Test{
				Test{
					Input: `seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
`,
					Part1: "35",
					Part2: "46",
				},
			},
		},
	})
}

type aoc202305 struct {
	AOC
}

func (aoc *aoc202305) parse(input *Input) ([]int, [][][3]int) {
	paragraph, _ := input.Paragraph()
	seeds := InputString(paragraph).Ints()

	mappings := [][][3]int{}
	for paragraph, ok := input.Paragraph(); ok; paragraph, ok = input.Paragraph() {
		mapping := [][3]int{}
		in := InputString(paragraph)
		for {
			dest, ok := in.Int()
			if !ok {
				break
			}
			src, _ := in.Int()
			rangeLen, _ := in.Int()
			mapping = append(mapping, [3]int{src, src + rangeLen, dest})
		}
		slices.SortFunc(mapping, func(a, b [3]int) int {
			return a[0] - b[0]
		})
		mappings = append(mappings, mapping)
	}
	return seeds, mappings
}

func (aoc *aoc202305) mapSeed(seed int, mappings [][][3]int) int {
	for _, mapping := range mappings {
		for _, m := range mapping {
			if seed < m[0] {
				break
			} else if seed < m[1] {
				seed = seed - m[0] + m[2]
				break
			}
		}
	}
	return seed
}

func (aoc *aoc202305) Part1(input *Input) string {
	seeds, mappings := aoc.parse(input)

	result := math.MaxInt
	for _, seed := range seeds {
		result = min(result, aoc.mapSeed(seed, mappings))
	}

	return IntResult(result)
}

func (aoc *aoc202305) mapRanges(ranges [][2]int, mappings [][][3]int) [][2]int {
	for _, mapping := range mappings {
		nextRanges := [][2]int{}
		for _, r := range ranges {
			for _, m := range mapping {
				if r[1] <= m[0] {
					break
				} else if r[0] >= m[1] {
					continue
				}
				if r[0] < m[0] {
					nextRanges = append(nextRanges, [2]int{r[0], m[0]})
					r = [2]int{m[0], r[1]}
				}
				if r[1] <= m[1] {
					r[0] = m[2] + r[0] - m[0]
					r[1] = m[2] + r[1] - m[0]
					break
				}
				nextRanges = append(nextRanges, [2]int{m[2] + r[0] - m[0], m[2] + m[1] - m[0]})
				r[0] = m[1]
			}
			if r[1] > r[0] {
				nextRanges = append(nextRanges, r)
			}
		}
		ranges = nextRanges
	}
	return ranges
}

func (aoc *aoc202305) Part2(input *Input) string {
	seeds, mappings := aoc.parse(input)

	result := math.MaxInt
	for i := 0; i < len(seeds); i += 2 {
		for _, r := range aoc.mapRanges([][2]int{{seeds[i], seeds[i] + seeds[i+1]}}, mappings) {
			result = min(result, r[0])
		}
	}

	return IntResult(result)
}
