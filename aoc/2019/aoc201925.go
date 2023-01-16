/*
--- Day 25: Cryostasis ---

As you approach Santa's ship, your sensors report two important details:

First, that you might be too late: the internal temperature is -40 degrees.

Second, that one faint life signature is somewhere on the ship.

The airlock door is locked with a code; your best option is to send in a small
droid to investigate the situation. You attach your ship to Santa's, break a
small hole in the hull, and let the droid run in before you seal it up again.
Before your ship starts freezing, you detach your ship and set it to
automatically stay within range of Santa's ship.

This droid can follow basic instructions and report on its surroundings; you
can communicate with it through an Intcode program (your puzzle input) running
on an ASCII-capable computer.

As the droid moves through its environment, it will describe what it
encounters. When it says Command?, you can give it a single instruction
terminated with a newline (ASCII code 10). Possible instructions are:

 - Movement via north, south, east, or west.
 - To take an item the droid sees in the environment, use the command take
   <name of item>. For example, if the droid reports seeing a red ball, you can
   pick it up with take red ball.
 - To drop an item the droid is carrying, use the command drop <name of item>.
   For example, if the droid is carrying a green ball, you can drop it with
   drop green ball.
 - To get a list of all of the items the droid is currently carrying, use the
   command inv (for "inventory").

Extra spaces or other characters aren't allowed - instructions must be provided
precisely.

Santa's ship is a Reindeer-class starship; these ships use pressure-sensitive
floors to determine the identity of droids and crew members. The standard
configuration for these starships is for all droids to weigh exactly the same
amount to make them easier to detect. If you need to get past such a sensor,
you might be able to reach the correct weight by carrying items from the
environment.

Look around the ship and see if you can find the password for the main airlock.
*/

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
