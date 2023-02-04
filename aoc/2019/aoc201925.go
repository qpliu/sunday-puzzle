package main

import (
	"io"
	"os"
)

func part1(canned []byte, script io.Reader, ic *intcode) {
	buf := [20]byte{}
	inp := canned
	showOutput := false
	ic.init()
	ic.interpIO(func() (bool, int64) {
		if len(inp) > 0 {
			ch := inp[0]
			inp = inp[1:]
			return false, int64(ch)
		}
		showOutput = true
		trace = false
		for i := 0; i < 20; i++ {
			script.Read(buf[i:i+1])
			if buf[i] == '\n' {
				if buf[0] == '@' {
					trace = true
					inp = buf[2:i+1]
					return false, int64(buf[1])
				}
				inp = buf[1:i+1]
				return false, int64(buf[0])
			}
		}
		inp = buf[1:]
		return false, int64(buf[0])
	}, func(out int64) bool {
		if showOutput {
			print(string([]byte{byte(out)}))
		}
		return false
	})
}

func main() {
	const canned = `east
south
south
take hologram
north
north
west
south
take mouse
east
take shell
west
west
take whirled peas
east
north
west
north
north
west
take semiconductor
east
south
west
south
take hypercube
north
east
south
west
take antenna
south
take spool of cat6
north
west
south
`
	part1([]byte(canned), os.Stdin, parseFile("input/25.txt"))
}

// For my input data:
// hypercube:     0x       1
// shell:         0x  800000
// whirled peas:  0x      80
// spool of cat6: 0x10000000
// mouse:         0x       2
// antenna:       0x    4000
// hologram:      0x 1000000
// semiconductor: 0x    1000
