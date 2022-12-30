/*
--- Day 14: One-Time Pad ---

In order to communicate securely with Santa while you're on this mission,
you've been using a one-time pad that you generate using a pre-agreed
algorithm. Unfortunately, you've run out of keys in your one-time pad, and so
you need to generate some more.

To generate keys, you first get a stream of random data by taking the MD5 of a
pre-arranged salt (your puzzle input) and an increasing integer index (starting
with 0, and represented in decimal); the resulting MD5 hash should be
represented as a string of lowercase hexadecimal digits.

However, not all of these MD5 hashes are keys, and you need 64 new keys for
your one-time pad. A hash is a key only if:

 - It contains three of the same character in a row, like 777. Only consider
   the first such triplet in a hash.
 - One of the next 1000 hashes in the stream contains that same character five
   times in a row, like 77777.

Considering future hashes for five-of-a-kind sequences does not cause those
hashes to be skipped; instead, regardless of whether the current hash is a key,
always resume testing for keys starting with the very next hash.

For example, if the pre-arranged salt is abc:

 - The first index which produces a triple is 18, because the MD5 hash of abc18
   contains ...cc38887a5.... However, index 18 does not count as a key for your
   one-time pad, because none of the next thousand hashes (index 19 through
   index 1018) contain 88888.
 - The next index which produces a triple is 39; the hash of abc39 contains
   eee. It is also the first key: one of the next thousand hashes (the one at
   index 816) contains eeeee.
 - None of the next six triples are keys, but the one after that, at index 92, is: it contains 999 and index 200 contains 99999.
Eventually, index 22728 meets all of the criteria to generate the 64th key.
So, using our example salt of abc, index 22728 produces the 64th key.

Given the actual salt in your puzzle input, what index produces your 64th one-time pad key?
*/

package main

import (
	"crypto/md5"
	"fmt"
	"os"
)

func has3(hash [md5.Size]byte) (bool, byte) {
	count := 0
	var digit byte
	for _, d := range hash {
		if count == 2 && (d>>4) == digit {
			return true, digit
		}
		if count == 1 && (d>>4) == digit && d&15 == digit {
			return true, digit
		}
		digit = d & 15
		count = 1
		if digit == d>>4 {
			count = 2
		}
	}
	return false, 0
}

func has5(hash [md5.Size]byte, digit byte) bool {
	count := 0
	for _, d := range hash {
		if d>>4 == digit {
			count++
			if count >= 5 {
				return true
			}
		} else {
			count = 0
		}
		if d&15 == digit {
			count++
			if count >= 5 {
				return true
			}
		} else {
			count = 0
		}
	}
	return false
}

type candidate struct {
	index    int
	digit    byte
	hash     [md5.Size]byte
	previous *candidate
}

func makeHash(salt string, i int) [md5.Size]byte {
	return md5.Sum([]byte(fmt.Sprintf("%s%d", salt, i)))
}

func generateKeys(salt string, makeHash func(string, int) [md5.Size]byte, limit int) *candidate {
	nkeys := 0
	var keys *candidate
	var candidates *candidate
	for i := 0; ; i++ {
		hash := makeHash(salt, i)
		isCandidate, digit := has3(hash)
		if !isCandidate {
			continue
		}

		prune := &candidate{previous: candidates}
		for p := prune; ; {
			if p.previous == nil {
				break
			} else if p.previous.index+1000 < i {
				p.previous = nil
				break
			}
			p = p.previous
		}
		candidates = prune.previous

		toTest := &candidate{previous: candidates}
		for t := toTest; ; {
			if t.previous == nil {
				break
			} else if has5(hash, t.previous.digit) {
				nkeys++
				newkey := t.previous
				t.previous = newkey.previous
				newkey.previous = keys
				keys = newkey
			} else {
				t = t.previous
			}
		}

		candidates = toTest.previous
		if nkeys < limit {
			candidates = &candidate{
				index:    i,
				digit:    digit,
				hash:     hash,
				previous: candidates,
			}
		} else if candidates == nil {
			return keys
		}
	}
}

func getNth(n int, keys *candidate) *candidate {
	current := &candidate{index: -1}
	// n^2 shouldn't be too bad when n is 64 or slightly more
	for i := 0; i < n; i++ {
		var min *candidate
		for k := keys; k != nil; k = k.previous {
			if k.index > current.index && (min == nil || k.index < min.index) {
				min = k
			}
		}
		current = min
	}
	return current
}

func runTests() {
	keys := generateKeys("abc", makeHash, 64)
	key1 := getNth(1, keys)
	if key1.index != 39 {
		panic(fmt.Sprintf("%d != 39", key1.index))
	}
	if ok, digit := has3(key1.hash); !ok || digit != 0xe {
		panic(fmt.Sprintf("%v,%x != e", ok, digit))
	}
	key2 := getNth(2, keys)
	if key2.index != 92 {
		panic(fmt.Sprintf("%d != 92", key2.index))
	}
	if ok, digit := has3(key2.hash); !ok || digit != 9 {
		panic(fmt.Sprintf("%v,%x != 9", ok, digit))
	}
	key64 := getNth(64, keys)
	if key64.index != 22728 {
		panic(fmt.Sprintf("%d != 22728", key64.index))
	}

	hash2 := fmt.Sprintf("%x", makeHash2("abc", 0))
	if hash2 != "a107ff634856bb300138cac6568c0f24" {
		panic(fmt.Sprintf("%s != a107ff634856bb300138cac6568c0f24", hash2))
	}
}

func makeHash2(salt string, i int) [md5.Size]byte {
	hash := md5.Sum([]byte(fmt.Sprintf("%s%d", salt, i)))
	for i := 0; i < 2016; i++ {
		hash = md5.Sum([]byte(fmt.Sprintf("%x", hash)))
	}
	return hash
}

func main() {
	if len(os.Args) < 2 {
		runTests()
	} else if len(os.Args) < 3 {
		keys := generateKeys(os.Args[1], makeHash, 64)
		key := getNth(64, keys)
		println(key.index)
	} else {
		keys := generateKeys(os.Args[2], makeHash2, 64)
		key := getNth(64, keys)
		println(key.index)
	}
}
