package main

import (
	"cmp"
	"slices"
	"strings"
)

func init() {
	Register(&aoc202021{
		AOC: AOC{
			Day:           21,
			InputFilename: "../2020/input/21.txt",
			Tests: []Test{
				Test{
					Input: `mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)
`,
					Part1: "5",
					Part2: "mxmxvkd,sqjhc,fvjkl",
				},
			},
		},
	})
}

type aoc202021 struct {
	AOC
}

func (aoc *aoc202021) parse(input *Input) Seq2[map[string]bool, map[string]bool] {
	return func(yield func(map[string]bool, map[string]bool) bool) {
		ingredients := map[string]bool{}
		allergens := map[string]bool{}
		ingredientsDone := false
		for word := range input.Words() {
			if word == "(contains" {
				ingredientsDone = true
				continue
			} else if !ingredientsDone {
				ingredients[word] = true
				continue
			} else if word[len(word)-1] == ',' {
				allergens[word[:len(word)-1]] = true
				continue
			} else if word[len(word)-1] == ')' {
				allergens[word[:len(word)-1]] = true
				if !yield(ingredients, allergens) {
					return
				}
				ingredients = map[string]bool{}
				ingredientsDone = false
				allergens = map[string]bool{}
			}
		}
	}
}

func (aoc *aoc202021) process(input *Input) (map[string]map[string]bool, map[string]map[string]bool, map[string]int) {
	possibles := map[string]map[string]bool{}
	impossibles := map[string]map[string]bool{}
	counts := map[string]int{}
	for ingredients, allergens := range aoc.parse(input) {
		for ingredient := range ingredients {
			counts[ingredient]++
			if impossibles[ingredient] == nil {
				impossibles[ingredient] = map[string]bool{}
			}
		}
		for allergen := range allergens {
			if possibles[allergen] == nil {
				possibles[allergen] = map[string]bool{}
			}
			possible := possibles[allergen]
			if len(possible) == 0 {
				for ing := range ingredients {
					possible[ing] = true
				}
			} else {
				for ing := range ingredients {
					if !possible[ing] {
						impossibles[ing][allergen] = true
					}
				}
			}
			for ing := range possible {
				if !ingredients[ing] {
					impossibles[ing][allergen] = true
					delete(possible, ing)
				}
			}
		}
	}

	return possibles, impossibles, counts
}

func (aoc *aoc202021) Part1(input *Input) string {
	possibles, impossibles, counts := aoc.process(input)
	result := 0
	n := len(possibles)
	for ing, impossible := range impossibles {
		for aller, possible := range possibles {
			if !possible[ing] {
				impossible[aller] = true
			}
		}
		if n == len(impossible) {
			result += counts[ing]
		}
	}
	return IntResult(result)
}

func (aoc *aoc202021) Part2(input *Input) string {
	possibles, _, _ := aoc.process(input)
	results := [][2]string{}
	for len(possibles) > 0 {
		for aller, possible := range possibles {
			if len(possible) == 1 {
				ing := ""
				for i := range possible {
					ing = i
				}
				results = append(results, [2]string{aller, ing})
				delete(possibles, aller)
				for _, p := range possibles {
					delete(p, ing)
				}
			}
		}
	}
	slices.SortFunc(results, func(a, b [2]string) int {
		return cmp.Compare(a[0], b[0])
	})
	result := []string{}
	for _, r := range results {
		result = append(result, r[1])
	}
	return strings.Join(result, ",")
}
