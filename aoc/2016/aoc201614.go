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
