package main
import (
	"math/rand"
)

func avg(counts []int) float64 {
	n := float64(len(counts))
	avg := float64(0)
	for _, count := range counts {
		avg += float64(count)/n
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
			if y < gaps[i][0] + minGap {
				if y > gaps[i][1] - minGap {
					gapCount--
					if gapCount <= 0 {
						return count
					}
					copy(gaps[i:gapCount], gaps[i+1:gapCount+1])
				} else {
					gaps[i][0] = y
				}
			} else if y > gaps[i][1] - minGap {
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
}
