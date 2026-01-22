package main

import (
	"strings"
)

func init() {
	Register(&aoc202004{
		AOC: AOC{
			Day:           4,
			InputFilename: "../2020/input/04.txt",
			Tests: []Test{
				Test{
					Input: `ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
`,
					Part1: "2",
					Part2: "",
				},
			},
		},
	})
}

type aoc202004 struct {
	AOC
}

func (aoc *aoc202004) parse(input *Input) Seq[map[string]string] {
	return func(yield func(map[string]string) bool) {
		for paragraph, ok := input.Paragraph(); ok; paragraph, ok = input.Paragraph() {
			passport := map[string]string{}
			for w := range InputString(paragraph).Words() {
				colon := strings.Index(w, ":")
				passport[w[:colon]] = w[colon+1:]
			}
			if !yield(passport) {
				return
			}
		}
	}
}

func (aoc *aoc202004) Part1(input *Input) string {
	result := 0
	for passport := range aoc.parse(input) {
		if _, ok := passport["byr"]; !ok {
			continue
		}
		if _, ok := passport["iyr"]; !ok {
			continue
		}
		if _, ok := passport["eyr"]; !ok {
			continue
		}
		if _, ok := passport["hgt"]; !ok {
			continue
		}
		if _, ok := passport["hcl"]; !ok {
			continue
		}
		if _, ok := passport["ecl"]; !ok {
			continue
		}
		if _, ok := passport["pid"]; !ok {
			continue
		}
		result++
	}
	return IntResult(result)
}

func (aoc *aoc202004) Part2(input *Input) string {
	badYear := func(yr string, atLeast, atMost int) bool {
		if len(yr) != 4 {
			return true
		}
		y, _ := InputString(yr).Int()
		return y < atLeast || y > atMost
	}

	badHeight := func(hgt string) bool {
		if len(hgt) < 4 {
			return true
		}
		h, _ := InputString(hgt).Int()
		switch hgt[len(hgt)-2:] {
		case "cm":
			return h < 150 || h > 193
		case "in":
			return h < 59 || h > 76
		default:
			return true
		}
	}

	badHair := func(hcl string) bool {
		if len(hcl) != 7 || hcl[0] != '#' {
			return true
		}
		for _, ch := range hcl[1:] {
			switch ch {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f':
			default:
				return true
			}
		}
		return false
	}

	badEye := func(ecl string) bool {
		switch ecl {
		case "amb", "blu", "brn", "gry", "grn", "hzl", "oth":
			return false
		default:
			return true
		}
	}

	badPID := func(pid string) bool {
		if len(pid) != 9 {
			return true
		}
		for _, ch := range pid {
			switch ch {
			case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
			default:
				return true
			}
		}
		return false
	}

	result := 0
	for passport := range aoc.parse(input) {
		if badYear(passport["byr"], 1920, 2002) {
			continue
		}
		if badYear(passport["iyr"], 2010, 2020) {
			continue
		}
		if badYear(passport["eyr"], 2020, 2030) {
			continue
		}
		if badHeight(passport["hgt"]) {
			continue
		}
		if badHair(passport["hcl"]) {
			continue
		}
		if badEye(passport["ecl"]) {
			continue
		}
		if badPID(passport["pid"]) {
			continue
		}
		result++
	}
	return IntResult(result)
}
