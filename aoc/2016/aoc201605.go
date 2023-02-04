package main

import (
	"crypto/md5"
	"fmt"
	"os"
)

func generate(doorID string) string {
	var result [8]byte
	index := 0
	for i := 0; ; i++ {
		hash := md5.Sum([]byte(fmt.Sprintf("%s%d", doorID, i)))
		if hash[0] == 0 && hash[1] == 0 && hash[2] < 16 {
			result[index] = hash[2]
			index++
			if index >= 8 {
				return fmt.Sprintf("%x%x%x%x%x%x%x%x", result[0], result[1], result[2], result[3], result[4], result[5], result[6], result[7])
			}
		}
	}
}

func generate2(doorID string) string {
	var result [8]byte
	var filled [8]bool
iterating:
	for i := 0; ; i++ {
		hash := md5.Sum([]byte(fmt.Sprintf("%s%d", doorID, i)))
		if hash[0] == 0 && hash[1] == 0 && hash[2] < 8 {
			index := int(hash[2])
			if filled[index] {
				continue
			}
			filled[index] = true
			result[index] = hash[3] >> 4
			for _, f := range filled {
				if !f {
					continue iterating
				}
			}
			return fmt.Sprintf("%x%x%x%x%x%x%x%x", result[0], result[1], result[2], result[3], result[4], result[5], result[6], result[7])
		}
	}
	
}

func main() {
	if len(os.Args) < 2 {
		if p := generate("abc"); p != "18f47a30" {
			panic(p)
		}
		if p := generate2("abc"); p != "05ace8e3" {
			panic(p)
		}
	} else if len(os.Args) < 3 {
		println(generate(os.Args[1]))
	} else {
		println(generate2(os.Args[2]))
	}
}
