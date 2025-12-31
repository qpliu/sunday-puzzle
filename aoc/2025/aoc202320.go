package main

func init() {
	Register(&aoc202320{
		AOC: AOC{
			Day:           20,
			InputFilename: "../2023/input/20.txt",
			Tests: []Test{
				Test{
					Input: `broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
`,
					Part1: "32000000",
					Part2: "",
				},
				Test{
					Input: `broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
`,
					Part1: "11687500",
					Part2: "",
				},
			},
		},
	})
}

type aoc202320 struct {
	AOC
}

type aoc202320_module struct {
	name     string
	flipflop bool
	inverter map[string]bool
	outputs  []string
}

func (aoc *aoc202320) parse(input *Input) map[string]*aoc202320_module {
	modules := map[string]*aoc202320_module{}
	for line, ok := input.Line(); ok; line, ok = input.Line() {
		inp := InputString(line)
		name, _ := inp.Word()
		var inverter map[string]bool
		if name[0] == '&' {
			inverter = map[string]bool{}
			name = name[1:]
		} else if name[0] == '%' {
			name = name[1:]
		}
		inp.Word()
		outputs := []string{}
		for out, ok := inp.Word(); ok; out, ok = inp.Word() {
			if out[len(out)-1] == ',' {
				out = out[:len(out)-1]
			}
			outputs = append(outputs, out)
		}
		modules[name] = &aoc202320_module{
			name:     name,
			flipflop: false,
			inverter: inverter,
			outputs:  outputs,
		}
	}
	for _, m := range modules {
		for _, out := range m.outputs {
			om := modules[out]
			if om != nil && om.inverter != nil {
				om.inverter[m.name] = false
			}
		}
	}
	return modules
}

func (aoc *aoc202320) Part1(input *Input) string {
	modules := aoc.parse(input)

	type pulse struct {
		src, dest string
		pulse     bool
	}
	nlo := 0
	nhi := 0
	for range 1000 {
		queue := NewQueue[pulse]()
		queue.Enqueue(pulse{"button", "broadcaster", false})
		for !queue.Empty() {
			p := queue.Dequeue()
			if p.pulse {
				nhi++
			} else {
				nlo++
			}
			m := modules[p.dest]
			if m == nil {
				continue
			}
			out := p.pulse
			if m.name == "broadcaster" {
			} else if m.inverter == nil {
				if p.pulse {
					continue
				}
				m.flipflop = !m.flipflop
				out = m.flipflop
			} else {
				m.inverter[p.src] = p.pulse
				if !p.pulse {
					out = true
				} else {
					out = false
					for _, mem := range m.inverter {
						if !mem {
							out = true
							break
						}
					}
				}
			}
			for _, dest := range m.outputs {
				queue.Enqueue(pulse{m.name, dest, out})
			}
		}
	}
	return IntResult(nlo * nhi)
}

func (aoc *aoc202320) Part2(input *Input) string {
	modules := aoc.parse(input)
	var torx *aoc202320_module
	for _, m := range modules {
		if len(m.outputs) == 1 && m.outputs[0] == "rx" {
			torx = m
			break
		}
	}
	if torx == nil || torx.inverter == nil {
		panic("unhandled input")
	}
	hitorx := map[string]int{}

	type pulse struct {
		src, dest string
		pulse     bool
	}
	for n := 1; ; n++ {
		queue := NewQueue[pulse]()
		queue.Enqueue(pulse{"button", "broadcaster", false})
		for !queue.Empty() {
			p := queue.Dequeue()
			m := modules[p.dest]
			if m == nil {
				continue
			}
			out := p.pulse
			if m.name == "broadcaster" {
			} else if m.inverter == nil {
				if p.pulse {
					continue
				}
				m.flipflop = !m.flipflop
				out = m.flipflop
			} else {
				m.inverter[p.src] = p.pulse
				if !p.pulse {
					out = true
				} else {
					if m == torx {
						hitorx[p.src] = n
						if len(hitorx) == len(torx.inverter) {
							result := 1
							for _, presses := range hitorx {
								result = LCM(result, presses)
							}
							return IntResult(result)
						}
					}
					out = false
					for _, mem := range m.inverter {
						if !mem {
							out = true
							break
						}
					}
				}
			}
			for _, dest := range m.outputs {
				queue.Enqueue(pulse{m.name, dest, out})
			}
		}
	}
}
