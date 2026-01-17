package main

import (
	"math"
)

func init() {
	Register(&aoc202117{
		AOC: AOC{
			Day:           17,
			InputFilename: "../2021/input/17.txt",
			Tests: []Test{
				Test{
					Input: `target area: x=20..30, y=-10..-5
`,
					Part1: "45",
					Part2: "112",
				},
			},
		},
	})
}

type aoc202117 struct {
	AOC
}

func (aoc *aoc202117) Part1(input *Input) string {
	config := input.Ints()
	x1, y1 := config[0], config[2]
	vy := -y1 - 1
	vx := int(math.Ceil((-1 + math.Sqrt(float64(8*x1)+1)) / 2))
	if vx > 2*vy+1 {
		panic("more work needed")
	}
	return IntResult(vy * (vy + 1) / 2)
}

func (aoc *aoc202117) Part2(input *Input) string {
	config := input.Ints()
	y1, y2 := config[2], config[3]
	targetTimes := map[int][]int{}
	tmax := 0

	for vy := y1; vy <= -y1-1; vy++ {
		rvy := float64(vy)
		t1 := int(math.Ceil(rvy + math.Sqrt(4*rvy*rvy+4*rvy-8*float64(y1)+1)/2 + 1/2))
		t2 := int(math.Floor(rvy + math.Sqrt(4*rvy*rvy+4*rvy-8*float64(y2)+1)/2 + 1/2))
		for t := t2; t <= t1; t++ {
			y := vy*t - t*(t-1)/2
			if y1 <= y && y <= y2 {
				targetTimes[t] = append(targetTimes[t], vy)
				tmax = max(tmax, t)
			}
		}
	}

	x1, x2 := config[0], config[1]
	result := 0
	for vx := 1; vx <= x2+1; vx++ {
		vys := map[int]bool{}
		for t := 0; t <= tmax; t++ {
			tt := min(t, vx)
			x := vx*tt - tt*(tt-1)/2
			if x1 <= x && x <= x2 {
				for _, vy := range targetTimes[t] {
					vys[vy] = true
				}
			}
		}
		result += len(vys)
	}
	return IntResult(result)
}
