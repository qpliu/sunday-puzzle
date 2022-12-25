/*
--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as
gifts for all the economically forward-thinking little girls and boys.

To do this, he needs to find MD5 hashes which, in hexadecimal, start with at
least five zeroes. The input to the MD5 hash is some secret key (your puzzle
input, given below) followed by a number in decimal. To mine AdventCoins, you
must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...)
that produces such a hash.

For example:

 - If your secret key is abcdef, the answer is 609043, because the MD5 hash of
   abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest
   such number to do so.
 - If your secret key is pqrstuv, the lowest number it combines with to make
   an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of
   pqrstuv1048970 looks like 000006136ef....
*/
package main

import (
	"crypto/md5"
	"fmt"
	"os"
)

func mine(key string) int {
	for i := 1; ; i++ {
		hash := md5.Sum([]byte(fmt.Sprintf("%s%d", key, i)))
		if hash[0] == 0 && hash[1] == 0 && hash[2] < 16 {
			return i
		}
	}
}

func main() {
	if len(os.Args) < 2 {
		if mine("abcdef") != 609043 {
			panic("abcdef")
		}
		if mine("pqrstuv") != 1048970 {
			panic("pqrstuv")
		}
	} else if len(os.Args) == 3 && os.Args[1] == "part2" {
		println(part2(os.Args[2]))
	} else {
		println(mine(os.Args[1]))
	}
}

func part2(key string) int {
	for i := 1; ; i++ {
		hash := md5.Sum([]byte(fmt.Sprintf("%s%d", key, i)))
		if hash[0] == 0 && hash[1] == 0 && hash[2] == 0 {
			return i
		}
	}
}
