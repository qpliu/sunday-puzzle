package main

import (
	"math/rand"
)

func avg(counts []int) float64 {
	n := float64(len(counts))
	avg := float64(0)
	for _, count := range counts {
		avg += float64(count) / n
	}
	return avg
}

func simulateOne() int {
	var gaps [10][2]float64
	count := 0
	gapCount := 1
	gaps[0][0] = 0
	gaps[0][1] = 1
	const minGap = 0.1
	for {
		y := rand.Float64()
		count++
		for i := 0; i < gapCount; i++ {
			if y < gaps[i][0] || y > gaps[i][1] {
				continue
			}
			if y < gaps[i][0]+minGap {
				if y > gaps[i][1]-minGap {
					gapCount--
					if gapCount <= 0 {
						return count
					}
					copy(gaps[i:gapCount], gaps[i+1:gapCount+1])
				} else {
					gaps[i][0] = y
				}
			} else if y > gaps[i][1]-minGap {
				gaps[i][1] = y
			} else {
				gaps[gapCount][0] = y
				gaps[gapCount][1] = gaps[i][1]
				gaps[i][1] = y
				gapCount++
			}
			break
		}
	}
}

func simulate(n int) {
	counts := make([]int, n)
	for i := 0; i < n; i++ {
		counts[i] = simulateOne()
	}
	println(n, avg(counts))
}

func main() {
	simulate(10)
	simulate(100)
	simulate(1000)
	simulate(10000)
	simulate(100000)
	simulate(1000000)
	simulate(10000000)
	extraCredit(10)
	extraCredit(100)
	extraCredit(1000)
	extraCredit(10000)
	extraCredit(100000)
	extraCredit(1000000)
}

type ecHold struct {
	x, y                                  float64
	reachableFromBottom, reachableFromTop bool
	canReach                              []int
}

func (hold *ecHold) climbableAfterPropogateReachable(fromBottom, fromTop bool, holds []ecHold) bool {
	if (fromBottom || hold.reachableFromBottom) && (fromTop || hold.reachableFromTop) {
		return true
	}
	if (fromBottom == hold.reachableFromBottom) && (fromTop == hold.reachableFromTop) {
		return false
	}
	if fromBottom && !hold.reachableFromBottom {
		hold.reachableFromBottom = true
		for _, i := range hold.canReach {
			if holds[i].climbableAfterPropogateReachable(fromBottom, fromTop, holds) {
				return true
			}
		}
	}
	if fromTop && !hold.reachableFromTop {
		hold.reachableFromTop = true
		for _, i := range hold.canReach {
			if holds[i].climbableAfterPropogateReachable(fromBottom, fromTop, holds) {
				return true
			}
		}
	}
	return false
}

func extraCreditOne() int {
	holds := make([]ecHold, 0, 200)
	count := 0
	const minDist = 0.1
	const minDist2 = minDist * minDist
	for {
		x := rand.Float64()
		y := rand.Float64()
		reachableFromBottom := y < minDist
		reachableFromTop := y > 1.0-minDist
		canReach := []int{}
		for i := range holds {
			dx := x - holds[i].x
			dy := y - holds[i].y
			if dx*dx+dy*dy < minDist2 {
				canReach = append(canReach, i)
				if holds[i].reachableFromBottom {
					reachableFromBottom = true
				}
				if holds[i].reachableFromTop {
					reachableFromTop = true
				}
				holds[i].canReach = append(holds[i].canReach, count)
			}
		}
		holds = append(holds, ecHold{x, y, false, false, canReach})
		if holds[count].climbableAfterPropogateReachable(reachableFromBottom, reachableFromTop, holds) {
			return count + 1
		}
		count++
	}
}

func extraCredit(n int) {
	counts := make([]int, n)
	for i := 0; i < n; i++ {
		counts[i] = extraCreditOne()
	}
	println(n, avg(counts))
}
