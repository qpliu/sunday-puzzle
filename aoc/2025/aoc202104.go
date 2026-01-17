package main

func init() {
	Register(&aoc202104{
		AOC: AOC{
			Day:           4,
			InputFilename: "../2021/input/04.txt",
			Tests: []Test{
				Test{
					Input: `7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

22 13 17 11  0
 8  2 23  4 24
21  9 14 16  7
 6 10  3 18  5
 1 12 20 15 19

 3 15  0  2 22
 9 18 13 17  5
19  8  7 25 23
20 11 10 24  4
14 21 16 12  6

14 21 17 24  4
10 16 15  9 19
18  8 23 26 20
22 11 13  6  5
 2  0 12  3  7
`,
					Part1: "4512",
					Part2: "1924",
				},
			},
		},
	})
}

type aoc202104 struct {
	AOC
}

func (aoc *aoc202104) parse(input *Input) (Seq[int], [][100]int) {
	numbers, _ := input.Paragraph()
	boards := [][100]int{}
	for paragraph := range input.Paragraphs() {
		board := [100]int{}
		i := 1
		for n := range InputString(paragraph).IntSeq() {
			board[n] = i
			i++
		}
		boards = append(boards, board)
	}
	return InputString(numbers).IntSeq(), boards
}

func (aoc *aoc202104) makeWinners() []int {
	return []int{
		(1 << 1) | (1 << 2) | (1 << 3) | (1 << 4) | (1 << 5),
		(1 << 6) | (1 << 7) | (1 << 8) | (1 << 9) | (1 << 10),
		(1 << 11) | (1 << 12) | (1 << 13) | (1 << 14) | (1 << 15),
		(1 << 16) | (1 << 17) | (1 << 18) | (1 << 19) | (1 << 20),
		(1 << 21) | (1 << 22) | (1 << 23) | (1 << 24) | (1 << 25),
		(1 << 1) | (1 << 6) | (1 << 11) | (1 << 16) | (1 << 21),
		(1 << 2) | (1 << 7) | (1 << 12) | (1 << 17) | (1 << 22),
		(1 << 3) | (1 << 8) | (1 << 13) | (1 << 18) | (1 << 23),
		(1 << 4) | (1 << 9) | (1 << 14) | (1 << 19) | (1 << 24),
		(1 << 5) | (1 << 10) | (1 << 15) | (1 << 20) | (1 << 25),
	}
}

func (aoc *aoc202104) score(n int, board [100]int, numbers int) int {
	score := 0
	for i, pos := range board {
		if pos > 0 && (1<<pos)&numbers == 0 {
			score += i
		}
	}
	return score * n
}

func (aoc *aoc202104) Part1(input *Input) string {
	numbers, boards := aoc.parse(input)
	winners := aoc.makeWinners()
	marks := make([]int, len(boards))
	for n := range numbers {
		for i := range boards {
			marks[i] |= 1 << boards[i][n]
			for _, w := range winners {
				if marks[i]&w == w {
					return IntResult(aoc.score(n, boards[i], marks[i]))
				}
			}
		}
	}
	panic("bad input")
}

func (aoc *aoc202104) Part2(input *Input) string {
	numbers, boards := aoc.parse(input)
	winners := aoc.makeWinners()
	marks := make([]int, len(boards))
	score := 0
	for n := range numbers {
		for i := len(boards) - 1; i >= 0; i-- {
			marks[i] |= 1 << boards[i][n]
			for _, w := range winners {
				if marks[i]&w == w {
					score = aoc.score(n, boards[i], marks[i])
					copy(boards[i:], boards[i+1:])
					boards = boards[:len(boards)-1]
					copy(marks[i:], marks[i+1:])
					marks = marks[:len(marks)-1]
					break
				}
			}
		}
	}
	return IntResult(score)
}
