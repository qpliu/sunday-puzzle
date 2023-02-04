package main

// I could modify intcode.go for cooperative multitasking or I could just
// use goroutines.

import (
	"sync"
)

type Network struct {
	lock   sync.Mutex
	queues [][][2]int64

	finished   bool
	lastPacket [2]int64
	waitGroup  sync.WaitGroup
}

const (
	readStateInit = iota
	readStateX
	readStateY

	writeStateAddr
	writeStateX
	writeStateY
)

func (nw *Network) runNode(initIC *intcode, addr int) {
	defer nw.waitGroup.Done()
	ic := &intcode{}
	ic.initFrom(initIC)
	readState := readStateInit
	writeState := writeStateAddr
	writeAddr := 0
	writeXY := [2]int64{}
	ic.interpIO(func() (bool, int64) {
		switch readState {
		case readStateInit:
			readState = readStateX
			return false, int64(addr)
		case readStateX:
			readState = readStateY
			nw.lock.Lock()
			defer nw.lock.Unlock()
			if len(nw.queues[addr]) == 0 {
				readState = readStateX
				return nw.finished, -1
			}
			return false, nw.queues[addr][0][0]
		case readStateY:
			readState = readStateX
			nw.lock.Lock()
			defer nw.lock.Unlock()
			xy := nw.queues[addr][0]
			copy(nw.queues[addr], nw.queues[addr][1:])
			nw.queues[addr] = nw.queues[addr][:len(nw.queues[addr])-1]
			return nw.finished, xy[1]
		default:
			panic("invalid readState")
		}
	}, func(out int64) bool {
		switch writeState {
		case writeStateAddr:
			writeState = writeStateX
			writeAddr = int(out)
			return false
		case writeStateX:
			writeState = writeStateY
			writeXY[0] = out
			return false
		case writeStateY:
			writeState = writeStateAddr
			writeXY[1] = out
			nw.lock.Lock()
			defer nw.lock.Unlock()
			if writeAddr >= len(nw.queues) {
				nw.finished = true
				nw.lastPacket = writeXY
			} else {
				nw.queues[writeAddr] = append(nw.queues[writeAddr], writeXY)
			}
			return nw.finished
		default:
			panic("invalid writeState")
		}
	})
}

func part1(nproc int, initIC *intcode) int64 {
	nw := &Network{}
	nw.queues = make([][][2]int64, nproc)
	nw.waitGroup.Add(nproc)
	for i := 0; i < nproc; i++ {
		go nw.runNode(initIC, i)
	}
	nw.waitGroup.Wait()
	return nw.lastPacket[1]
}

type Node struct {
	ic *intcode
	readState int
	inputQueue [][2]int64
	writeState int
	writeAddr  int
	writePacket [2]int64
}

type NAT struct {
	nodes []*Node
	packet255 [2]int64
	lastRestart [2]int64
}

func (nat *NAT) init(nproc int, initIC *intcode) {
	nat.nodes = make([]*Node, nproc)
	for i := range nat.nodes {
		node := &Node{}
		nat.nodes[i] = node
		node.ic = &intcode{}
		node.ic.initFrom(initIC)
		node.readState = readStateInit
		node.writeState = writeStateAddr
	}
}

func (nat *NAT) part1() int64 {
	for {
		for i, node := range nat.nodes {
			switch node.ic.restartState {
			case icInit:
				node.ic.interpCoop(0)
			case icHalted:
			case icInput:
				switch node.readState {
				case readStateInit:
					node.readState = readStateX
					node.ic.interpCoop(int64(i))
				case readStateX:
					if len(node.inputQueue) == 0 {
						node.readState = readStateX
						node.ic.interpCoop(-1)
					} else {
						node.readState = readStateY
						node.ic.interpCoop(node.inputQueue[0][0])
					}
				case readStateY:
					node.readState = readStateX
					node.ic.interpCoop(node.inputQueue[0][1])
					copy(node.inputQueue, node.inputQueue[1:])
					node.inputQueue = node.inputQueue[:len(node.inputQueue)-1]
				default:
					panic("node.readState")
				}
			case icOutput:
				switch node.writeState {
				case writeStateAddr:
					node.writeState = writeStateX
					node.writeAddr = int(node.ic.output)
					node.ic.interpCoop(0)
				case writeStateX:
					node.writeState = writeStateY
					node.writePacket[0] = node.ic.output
					node.ic.interpCoop(0)
				case writeStateY:
					node.writeState = writeStateAddr
					node.writePacket[1] = node.ic.output
					if node.writeAddr == 255 {
						return node.writePacket[1]
					} else if node.writeAddr >= 0 && node.writeAddr < len(nat.nodes) {
						nat.nodes[node.writeAddr].inputQueue = append(nat.nodes[node.writeAddr].inputQueue, node.writePacket)
					}
					node.ic.interpCoop(0)
				}
			default:
				panic("node.ic.restartState")
			}
		}
	}
}

func (nat *NAT) part2() int64 {
	deadlockCount := 0
	for {
		deadlock := true
		for i, node := range nat.nodes {
			switch node.ic.restartState {
			case icInit:
				deadlock = false
				node.ic.interpCoop(0)
			case icHalted:
			case icInput:
				switch node.readState {
				case readStateInit:
					deadlock = false
					node.readState = readStateX
					node.ic.interpCoop(int64(i))
				case readStateX:
					if len(node.inputQueue) == 0 {
						node.readState = readStateX
						node.ic.interpCoop(-1)
					} else {
						deadlock = false
						node.readState = readStateY
						node.ic.interpCoop(node.inputQueue[0][0])
					}
				case readStateY:
					deadlock = false
					node.readState = readStateX
					node.ic.interpCoop(node.inputQueue[0][1])
					copy(node.inputQueue, node.inputQueue[1:])
					node.inputQueue = node.inputQueue[:len(node.inputQueue)-1]
				default:
					panic("node.readState")
				}
			case icOutput:
				deadlock = false
				switch node.writeState {
				case writeStateAddr:
					node.writeState = writeStateX
					node.writeAddr = int(node.ic.output)
					node.ic.interpCoop(0)
				case writeStateX:
					node.writeState = writeStateY
					node.writePacket[0] = node.ic.output
					node.ic.interpCoop(0)
				case writeStateY:
					node.writeState = writeStateAddr
					node.writePacket[1] = node.ic.output
					if node.writeAddr == 255 {
						nat.packet255 = node.writePacket
					} else if node.writeAddr >= 0 && node.writeAddr < len(nat.nodes) {
						nat.nodes[node.writeAddr].inputQueue = append(nat.nodes[node.writeAddr].inputQueue, node.writePacket)
					}
					node.ic.interpCoop(0)
				}
			default:
				panic("node.ic.restartState")
			}
		}
		if deadlock {
			deadlockCount++
		} else {
			deadlockCount = 0
		}
		if deadlockCount > 1 {
			if nat.lastRestart[1] == nat.packet255[1] {
				return nat.packet255[1]
			}
			nat.lastRestart = nat.packet255
			nat.nodes[0].inputQueue = append(nat.nodes[0].inputQueue, nat.packet255)
		}
	}
}

func main() {
	const nproc = 50
	initIC := parseFile("input/23.txt")
	println("Part 1: ", part1(nproc, initIC))
	nat := &NAT{}
	nat.init(nproc, initIC)
	println("Part 1: ", nat.part1())
	nat.init(nproc, initIC)
	println("Part 2: ", nat.part2())
}
