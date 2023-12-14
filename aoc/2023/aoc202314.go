package main

import (
	"os"
	"sort"
	"time"
)

// My solution in Haskell is pretty slow - almost 10 seconds for part 2.
// This is much faster - about 22 milliseconds for part 2.

// My input data has 1993 round boulders on a 100×100 platform
const (
	w = 100
	h = 100
	n = 1993
	part = 2
)

var (
	nTable [w*h]int
	wTable [w*h]int
	sTable [w*h]int
	eTable [w*h]int

	boulders [n]int
)

func load() int {
	l := 0
	for _, i := range boulders {
		l += i/w
	}
	return n*h - l
}

func nTilt() {
	var offsets [w*h]int
	for i, b := range boulders {
		t := nTable[b]
		boulders[i] = t + offsets[t]
		offsets[t] += w
	}
}

func wTilt() {
	var offsets [w*h]int
	for i, b := range boulders {
		t := wTable[b]
		boulders[i] = t + offsets[t]
		offsets[t]++
	}
}

func sTilt() {
	var offsets [w*h]int
	for i, b := range boulders {
		t := sTable[b]
		boulders[i] = t - offsets[t]
		offsets[t] += w
	}
}

func eTilt() {
	var offsets [w*h]int
	for i, b := range boulders {
		t := eTable[b]
		boulders[i] = t - offsets[t]
		offsets[t]++
	}
}

func readInput() {
	f, err := os.Open("input/14.txt")
	if err != nil {
		panic(err.Error())
	}
	defer f.Close()

	var buffer [(w+1)*h]byte
	if n, err := f.Read(buffer[:]); err != nil {
		panic(err.Error())
	} else if n != (w+1)*h {
		panic("unexpected input size")
	}

	var cubes [w*h]bool
	iboulder := 0
	for i, ch := range buffer {
		switch ch {
		case '\n':
		case '.':
		case 'O':
			boulders[iboulder] = i%(w+1) + (i/(w+1))*w
			iboulder++
		case '#':
			cubes[i%(w+1) + (i/(w+1))*w] = true
		default:
			panic("unexpected input")
		}
	}

	for col := 0; col < w; col++ {
		target := col
		for row := 0; row < h; row++ {
			i := col + row*w
			nTable[i] = target
			if cubes[i] {
				target = i + w
			}
		}
	}

	if part == 1 {
		return
	}

	for row := 0; row < h; row++ {
		target := row*w
		for col := 0; col < w; col++ {
			i := col + row*w
			wTable[i] = target
			if cubes[i] {
				target = i + 1
			}
		}
	}

	for col := 0; col < w; col++ {
		target := col + (h-1)*w
		for row := h-1; row >= 0; row-- {
			i := col + row*w
			sTable[i] = target
			if cubes[i] {
				target = i - w
			}
		}
	}

	for row := 0; row < h; row++ {
		target := (row+1)*w - 1
		for col := w-1; col >= 0; col-- {
			i := col + row*w
			eTable[i] = target
			if cubes[i] {
				target = i - 1
			}
		}
	}
}

func main() {
	start := time.Now()
	defer func() {
		end := time.Now()
		println(start.Format("15:04:05.000000000"),end.Format("15:04:05.000000000"),float64(end.Sub(start))/1000000000)
	}()
	switch part {
	case 1:
		readInput()
		nTilt()
		println(load())
	default:
		readInput()
		states := make(map[[n]int]int)
		i := 0
		for {
			if n, ok := states[boulders]; ok {
				target := (1000000000-n) % (i-n) + n
				for b, j := range states {
					if j == target {
						boulders = b
						break
					}
				}
				println(n, i, load())
				break
			}
			states[boulders] = i
			nTilt()
			wTilt()
			sTilt()
			eTilt()
			sort.Ints(boulders[:])
			i++
		}
	}
}
