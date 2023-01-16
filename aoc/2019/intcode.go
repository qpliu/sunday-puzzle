package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
)

type icState int

const (
	icInit = icState(iota)
	icHalted
	icInput
	icOutput
	
)

type intcode struct {
	code         []int64
	mem          []int64
	ip           int64
	relativebase int64
	restartState icState
	output       int64
}

func parseString(input string) *intcode {
	return parse(bytes.NewBufferString(input))
}

func parseFile(filename string) *intcode {
	b, err := os.ReadFile(filename)
	if err != nil {
		panic(err.Error())
	}
	return parse(bytes.NewBuffer(b))
}

func parse(input io.Reader) *intcode {
	mem := []int64{}
	var i int64
	if _, err := fmt.Fscanf(input, "%d", &i); err != nil {
		panic(err.Error())
	}
	mem = append(mem, i)
	for {
		if _, err := fmt.Fscanf(input, ",%d", &i); err != nil {
			break
		}
		mem = append(mem, i)
	}
	return &intcode{mem, []int64{}, 0, 0, icInit, 0}
}

func (ic *intcode) getloc(index int, forWrite bool) int64 {
	mode := ic.mem[ic.ip] / 10
	for i := 0; i < index; i++ {
		mode /= 10
	}
	mode %= 10

	var loc int64
	switch mode {
	case 0:
		loc = ic.mem[ic.ip+int64(index)]
	case 1:
		loc = ic.ip + int64(index)
	case 2:
		loc = ic.mem[ic.ip+int64(index)] + ic.relativebase
	default:
		panic("unknown mode")
	}

	if forWrite && loc >= int64(len(ic.mem)) {
		mem := make([]int64, loc+10000)
		copy(mem, ic.mem)
		ic.mem = mem
	}

	return loc
}

func (ic *intcode) getarg(index int) int64 {
	i := ic.getloc(index, false)
	if i < int64(len(ic.mem)) {
		return ic.mem[i]
	} else {
		return 0
	}
}

func (ic *intcode) putarg(index int, value int64) {
	i := ic.getloc(index, true)
	ic.mem[i] = value
}

func (ic *intcode) init() {
	ic.mem = make([]int64, len(ic.code))
	copy(ic.mem, ic.code)
	ic.ip = 0
	ic.relativebase = 0
	ic.restartState = icInit
}

func (ic *intcode) initFrom(fromIC *intcode) {
	ic.code = fromIC.code
	ic.init()
}

func (ic *intcode) interp(input []int64) []int64 {
	output := []int64{}
	ic.interpIO(func() (bool, int64) {
		inp := input[0]
		input = input[1:]
		return false, inp
	}, func(out int64) bool {
		output = append(output, out)
		return false
	})
	return output
}

func (ic *intcode) interpIO(input func() (bool, int64), output func(int64) bool) {
	in := int64(0)
	for {
		switch res, out := ic.interpCoop(in); res {
		case icHalted:
			return
		case icInput:
			exit, newIn := input()
			in = newIn
			if exit {
				return
			}
		case icOutput:
			exit := output(out)
			if exit {
				return
			}
		}
	}
}

func (ic *intcode) interpCoop(input int64) (icState, int64) {
	if ic.restartState == icInput && ic.mem[ic.ip] % 100 == 3 {
		ic.putarg(1, input)
		ic.ip += 2
	}
	for {
		switch ic.mem[ic.ip] % 100 {
		case 1:
			ic.putarg(3, ic.getarg(1)+ic.getarg(2))
			ic.ip += 4
		case 2:
			ic.putarg(3, ic.getarg(1)*ic.getarg(2))
			ic.ip += 4
		case 3:
			ic.restartState = icInput
			return icInput, 0
		case 4:
			out := ic.getarg(1)
			ic.ip += 2
			ic.restartState = icOutput
			ic.output = out
			return icOutput, out
		case 5:
			if ic.getarg(1) != 0 {
				ic.ip = ic.getarg(2)
			} else {
				ic.ip += 3
			}
		case 6:
			if ic.getarg(1) == 0 {
				ic.ip = ic.getarg(2)
			} else {
				ic.ip += 3
			}
		case 7:
			if ic.getarg(1) < ic.getarg(2) {
				ic.putarg(3, 1)
			} else {
				ic.putarg(3, 0)
			}
			ic.ip += 4
		case 8:
			if ic.getarg(1) == ic.getarg(2) {
				ic.putarg(3, 1)
			} else {
				ic.putarg(3, 0)
			}
			ic.ip += 4
		case 9:
			ic.relativebase += ic.getarg(1)
			ic.ip += 2
		case 99:
			return icHalted, 0
		default:
			panic(fmt.Sprintf("unknown opcode %d at %d", ic.mem[ic.ip], ic.ip))
		}
	}
}
