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
