package main

import (
	"bytes"
	"io"
	"os"
)

func run(ic *intcode, script io.Reader) {
	buf := [1]byte{}
	ic.init()
	ic.interpIO(func() (bool, int64) {
		script.Read(buf[:])
		return false, int64(buf[0])
	}, func(out int64) bool {
		if out < 128 {
			print(string([]byte{byte(out)}))
			return false
		} else {
			println("Damage is", out)
			return false
		}
	})
}

func main() {
	ic := parseFile("input/21.txt")
	if len(os.Args) < 2 {
		// Part 1:
		// jump if see
		//   .??? - a
		//   #..# - (!b & !c & d)
		//   ##.# - (b & !c & d)
		run(ic, bytes.NewBufferString("NOT C J\nOR D T\nAND D J\nNOT A T\nOR T J\nWALK\n"))

		// Part 2:
		// jump if ???##? or ???#.??#?
		// but not if ?##?
		// but still jump if .
		run(ic, bytes.NewBufferString("OR D J\nOR E T\nOR H T\nAND T J\nNOT B T\nNOT T T\nAND C T\nNOT T T\nAND T J\nNOT A T\nOR T J\nRUN\n"))
	} else {
		run(ic, os.Stdin)
	}
}
