/*
--- Day 9: Marble Mania ---

You talk to the Elves while you wait for your navigation system to initialize.
To pass the time, they introduce you to their favorite marble game.

The Elves play this game by taking turns arranging the marbles in a circle
according to very particular rules. The marbles are numbered starting with 0
and increasing by 1 until every marble has a number.

First, the marble numbered 0 is placed in the circle. At this point, while it
contains only a single marble, it is still a circle: the marble is both
clockwise from itself and counter-clockwise from itself. This marble is
designated the current marble.

Then, each Elf takes a turn placing the lowest-numbered remaining marble into
the circle between the marbles that are 1 and 2 marbles clockwise of the
current marble. (When the circle is large enough, this means that there is one
marble between the marble that was just placed and the current marble.) The
marble that was just placed then becomes the current marble.

However, if the marble that is about to be placed has a number which is a
multiple of 23, something entirely different happens. First, the current player
keeps the marble they would have placed, adding it to their score. In addition,
the marble 7 marbles counter-clockwise from the current marble is removed from
the circle and also added to the current player's score. The marble located
immediately clockwise of the marble that was removed becomes the new current
marble.

For example, suppose there are 9 players. After the marble with value 0 is
placed in the middle, each player (shown in square brackets) takes a turn. The
result of each of those turns would produce circles of marbles like this, where
clockwise is to the right and the resulting current marble is in parentheses:

| [-] (0)
| [1]  0 (1)
| [2]  0 (2) 1
| [3]  0  2  1 (3)
| [4]  0 (4) 2  1  3
| [5]  0  4  2 (5) 1  3
| [6]  0  4  2  5  1 (6) 3
| [7]  0  4  2  5  1  6  3 (7)
| [8]  0 (8) 4  2  5  1  6  3  7
| [9]  0  8  4 (9) 2  5  1  6  3  7
| [1]  0  8  4  9  2(10) 5  1  6  3  7
| [2]  0  8  4  9  2 10  5(11) 1  6  3  7
| [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7
| [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7
| [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7
| [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
| [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15
| [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15
| [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15
| [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15
| [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15
| [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15
| [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15
| [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15
| [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15
| [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

The goal is to be the player with the highest score after the last marble is
used up. Assuming the example above ends after the marble numbered 25, the
winning score is 23+9=32 (because player 5 kept marble 23 and removed marble 9,
while no other player got any points in this very short example game).

Here are a few more examples:

 - 10 players; last marble is worth 1618 points: high score is 8317
 - 13 players; last marble is worth 7999 points: high score is 146373
 - 17 players; last marble is worth 1104 points: high score is 2764
 - 21 players; last marble is worth 6111 points: high score is 54718
 - 30 players; last marble is worth 5807 points: high score is 37305

What is the winning Elf's score?
*/

// Should do much better on memory than the Haskell code.
// Still unsatisfactorily slow, but should be barely feasible.

package main

import (
	"os"
	"strconv"
)

func maxscore(scores []int) int {
	max := 0
	for _, s := range scores {
		if s > max {
			max = s
		}
	}
	return max
}

func step(m, size int, buf1, buf2 []int) {
	buf2[0] = buf1[20]
	buf2[1] = m + 19
	buf2[2] = buf1[21]
	buf2[3] = m + 20
	buf2[4] = buf1[22]
	buf2[5] = m + 21
	buf2[6] = buf1[23]
	buf2[7] = m + 22
	copy(buf2[8:], buf1[24:size])
	buf2[size-37] = buf1[1]
	buf2[size-36] = buf1[2]
	buf2[size-35] = m + 1
	buf2[size-34] = buf1[3]
	buf2[size-33] = m + 2
	buf2[size-32] = buf1[4]
	buf2[size-31] = m + 3
	buf2[size-30] = buf1[5]
	buf2[size-29] = m + 4
	buf2[size-28] = buf1[6]
	buf2[size-27] = m + 5
	buf2[size-26] = buf1[7]
	buf2[size-25] = m + 6
	buf2[size-24] = buf1[8]
	buf2[size-23] = m + 7
	buf2[size-22] = buf1[9]
	buf2[size-21] = m + 8
	buf2[size-20] = buf1[10]
	buf2[size-19] = m + 9
	buf2[size-18] = buf1[11]
	buf2[size-17] = m + 10
	buf2[size-16] = buf1[12]
	buf2[size-15] = m + 11
	buf2[size-14] = buf1[13]
	buf2[size-13] = m + 12
	buf2[size-12] = buf1[14]
	buf2[size-11] = m + 13
	buf2[size-10] = buf1[15]
	buf2[size-9] = m + 14
	buf2[size-8] = buf1[16]
	buf2[size-7] = m + 15
	buf2[size-6] = buf1[17]
	buf2[size-5] = m + 16
	buf2[size-4] = buf1[18]
	buf2[size-3] = m + 17
	buf2[size-2] = buf1[19]
	buf2[size-1] = m + 18
}

func play(nplayers, max int) {
	maxsize := (max/23)*21 + 23
	buf1 := make([]int, maxsize)
	buf2 := make([]int, maxsize)
	scores := make([]int, nplayers)

	scores[23%nplayers] = 23 + 9

	size := 44
	m := 46
	copy(buf1, []int{17, 42, 4, 43, 18, 44, 19, 45, 2, 24, 20, 25, 10, 26, 21, 27, 5, 28, 22, 29, 11, 30, 1, 31, 12, 32, 6, 33, 13, 34, 3, 35, 14, 36, 7, 37, 15, 38, 0, 39, 16, 40, 8, 41})
	for {
		scores[m%nplayers] += m + buf1[0]
		size += 21
		step(m, size, buf1, buf2)
		m += 23
		if m > max {
			println(nplayers, max, m-23, buf1[0], maxscore(scores))
			break
		}

		scores[m%nplayers] += m + buf2[0]
		size += 21
		step(m, size, buf2, buf1)
		m += 23
		if m > max {
			println(nplayers, max, m-23, buf2[0], maxscore(scores))
			break
		}
		if m%(46*1000) == 0 {
			println(m-23, buf2[0])
		}
	}
}

func main() {
	if len(os.Args) < 3 {
		println("expect", 8317)
		play(10, 1618)
		println("expect", 146373)
		play(13, 7999)
		println("expect", 2764)
		play(17, 1104)
		println("expect", 54718)
		play(21, 6111)
		println("expect", 37305)
		play(30, 5807)
	} else {
		nplayers, _ := strconv.Atoi(os.Args[1])
		max, _ := strconv.Atoi(os.Args[2])
		play(nplayers, max)
	}
}
