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
