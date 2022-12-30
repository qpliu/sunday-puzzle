/*
--- Day 5: How About a Nice Game of Chess? ---

You are faced with a security door designed by Easter Bunny engineers that seem
to have acquired most of their security knowledge by watching hacking movies.

The eight-character password for the door is generated one character at a time
by finding the MD5 hash of some Door ID (your puzzle input) and an increasing
integer index (starting with 0).

A hash indicates the next character in the password if its hexadecimal
representation starts with five zeroes. If it does, the sixth character in the
hash is the next character of the password.

For example, if the Door ID is abc:

 - The first index which produces a hash that starts with five zeroes is
   3231929, which we find by hashing abc3231929; the sixth character of the
   hash, and thus the first character of the password, is 1.
 - 5017308 produces the next interesting hash, which starts with 000008f82...,
   so the second character of the password is 8.
 - The third time a hash starts with five zeroes is for abc5278568, discovering
   the character f.

In this example, after continuing this search a total of eight times, the
password is 18f47a30.

Given the actual Door ID, what is the password?
*/

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
