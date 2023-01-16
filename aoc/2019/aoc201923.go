/*
--- Day 23: Category Six ---

The droids have finished repairing as much of the ship as they can. Their
report indicates that this was a Category 6 disaster - not because it was that
bad, but because it destroyed the stockpile of Category 6 network cables as
well as most of the ship's network infrastructure.

You'll need to rebuild the network from scratch.

The computers on the network are standard Intcode computers that communicate by
sending packets to each other. There are 50 of them in total, each running a
copy of the same Network Interface Controller (NIC) software (your puzzle
input). The computers have network addresses 0 through 49; when each computer
boots up, it will request its network address via a single input instruction.
Be sure to give each computer a unique network address.

Once a computer has received its network address, it will begin doing work and
communicating over the network by sending and receiving packets. All packets
contain two values named X and Y. Packets sent to a computer are queued by the
recipient and read in the order they are received.

To send a packet to another computer, the NIC will use three output
instructions that provide the destination address of the packet followed by its
X and Y values. For example, three output instructions that provide the values
10, 20, 30 would send a packet with X=20 and Y=30 to the computer with address
10.

To receive a packet from another computer, the NIC will use an input
instruction. If the incoming packet queue is empty, provide -1. Otherwise,
provide the X value of the next packet; the computer will then use a second
input instruction to receive the Y value for the same packet. Once both values
of the packet are read in this way, the packet is removed from the queue.

Note that these input and output instructions never block. Specifically, output
instructions do not wait for the sent packet to be received - the computer
might send multiple packets before receiving any. Similarly, input instructions
do not wait for a packet to arrive - if no packet is waiting, input
instructions should receive -1.

Boot up all 50 computers and attach them to your network. What is the Y value
of the first packet sent to address 255?

--- Part Two ---

Packets sent to address 255 are handled by a device called a NAT (Not Always
Transmitting). The NAT is responsible for managing power consumption of the
network by blocking certain packets and watching for idle periods in the
computers.

If a packet would be sent to address 255, the NAT receives it instead. The NAT
remembers only the last packet it receives; that is, the data in each packet it
receives overwrites the NAT's packet memory with the new packet's X and Y
values.

The NAT also monitors all computers on the network. If all computers have empty
incoming packet queues and are continuously trying to receive packets without
sending packets, the network is considered idle.

Once the network is idle, the NAT sends only the last packet it received to
address 0; this will cause the computers on the network to resume activity. In
this way, the NAT can throttle power consumption of the network when the ship
needs power in other areas.

Monitor packets released to the computer at address 0 by the NAT. What is the
first Y value delivered by the NAT to the computer at address 0 twice in a row?
*/

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
