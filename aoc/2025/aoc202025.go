package main

import (
	"runtime"
	"sync"
)

func init() {
	Register(&aoc202025{
		AOC: AOC{
			Day:           25,
			InputFilename: "../2020/input/25.txt",
			Tests: []Test{
				Test{
					Input: `5764801
17807724
`,
					Part1: "14897079",
					Part2: "",
				},
			},
		},
	})
}

type aoc202025 struct {
	AOC
}

func (aoc *aoc202025) Part1(input *Input) string {
	const r = 20201227
	// cardPublicKey = 7^c mod 20201227
	// doorPublicKey = 7^d mod 20201227
	// encryptionKey = 7^(c*d) mod 20201227
	publicKeys := input.Ints()
	encryptionKey := -1

	mutex := &sync.Mutex{}
	wg := &sync.WaitGroup{}
	wg.Add(1)

	finish := func(privateKey, publicKey int) {
		var pow func(x, n int) int
		pow = func(x, n int) int {
			if n == 0 {
				return 1
			}
			x2 := pow(x, n/2)
			if n%2 == 0 {
				return (x2 * x2) % r
			} else {
				return (x * ((x2 * x2) % r)) % r
			}
		}
		mutex.Lock()
		defer mutex.Unlock()
		if encryptionKey < 0 {
			publicKey = pow(publicKey, privateKey)
			encryptionKey = publicKey
			wg.Done()
		}
	}

	step := 1
	n := runtime.NumCPU()
	for range n {
		step *= 7
		step %= r
	}
	privateKeyStart := 0
	start := 1
	for range n {
		privateKeyStart++
		start *= 7
		start %= r
		go func(privateKey, publicKey int) {
			for range 1000 {
				for {
					if publicKey == publicKeys[0] {
						finish(privateKey, publicKeys[1])
						return
					}
					if publicKey == publicKeys[1] {
						finish(privateKey, publicKeys[0])
						return
					}
					privateKey += n
					publicKey *= step
					publicKey %= r
				}
				mutex.Lock()
				done := encryptionKey >= 0
				mutex.Unlock()
				if done {
					return
				}
			}
		}(privateKeyStart, start)
	}

	wg.Wait()
	return IntResult(encryptionKey)
}

func (aoc *aoc202025) Part2(input *Input) string {
	return ""
}
