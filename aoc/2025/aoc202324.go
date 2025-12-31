package main

import (
	"math/big"
	"runtime"
)

func init() {
	Register(&aoc202324{
		AOC: AOC{
			Day:           24,
			InputFilename: "../2023/input/24.txt",
			Tests: []Test{
				Test{
					Input: `19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
`,
					Part1: "2",
					Part2: "47",
				},
			},
		},
	})
}

type aoc202324 struct {
	AOC
}

func (aoc *aoc202324) parse(limit int, input *Input) [][6]*big.Rat {
	hail := [][6]*big.Rat{}
	for range limit {
		px, _ := input.Int()
		py, _ := input.Int()
		pz, _ := input.Int()
		vx, _ := input.Int()
		vy, _ := input.Int()
		vz, ok := input.Int()
		if !ok {
			return hail
		}
		hail = append(hail, [6]*big.Rat{
			big.NewRat(int64(px), 1),
			big.NewRat(int64(py), 1),
			big.NewRat(int64(pz), 1),
			big.NewRat(int64(vx), 1),
			big.NewRat(int64(vy), 1),
			big.NewRat(int64(vz), 1),
		})
	}
	return hail
}

func (aoc *aoc202324) part1(minXY, maxXY int, input *Input) string {
	hail := aoc.parse(1000, input)
	in := make(chan int)
	out := make(chan int)
	r := func(n int) *big.Rat {
		return big.NewRat(int64(n), 1)
	}
	minRat := r(minXY)
	maxRat := r(maxXY)
	for range runtime.NumCPU() {
		go func() {
			result := 0
			tmp1, tmp2 := r(0), r(0)
			x, y := r(0), r(0)
			dx, dy := r(0), r(0)
			for i := range in {
				x1 := hail[i][0]
				y1 := hail[i][1]
				vx1 := hail[i][3]
				vy1 := hail[i][4]
				for j := range i {
					x2 := hail[j][0]
					y2 := hail[j][1]
					vx2 := hail[j][3]
					vy2 := hail[j][4]

					tmp1.Mul(vx2, vy1)
					tmp2.Mul(vx1, vy2)
					dx.Sub(tmp1, tmp2)
					// dx = vx2*vy1 - vx1*vy2

					tmp1.Mul(vy2, vx1)
					tmp2.Mul(vy1, vx2)
					dy.Sub(tmp1, tmp2)
					// dy = vy2*vx1 - vy1*vx2
					if dx.Sign() == 0 { // parallel
						continue
					}

					tmp1.Mul(vx2, vy1)
					tmp1.Mul(tmp1, x1)
					tmp2.Mul(vx1, vx2)
					tmp2.Mul(tmp2, y1)
					tmp1.Sub(tmp1, tmp2)
					tmp2.Mul(vx1, vy2)
					tmp2.Mul(tmp2, x2)
					tmp1.Sub(tmp1, tmp2)
					tmp2.Mul(vx1, vx2)
					tmp2.Mul(tmp2, y2)
					tmp1.Add(tmp1, tmp2)
					x.Quo(tmp1, dx)
					// x := (vx2*vy1*x1-vx1*vx2*y1-vx1*vy2*x2+vx1*vx2*y2)/dx
					if x.Cmp(minRat) < 0 || x.Cmp(maxRat) > 0 {
						continue
					}

					tmp1.Mul(vy2, vx1)
					tmp1.Mul(tmp1, y1)
					tmp2.Mul(vy1, vy2)
					tmp2.Mul(tmp2, x1)
					tmp1.Sub(tmp1, tmp2)
					tmp2.Mul(vy1, vx2)
					tmp2.Mul(tmp2, y2)
					tmp1.Sub(tmp1, tmp2)
					tmp2.Mul(vy1, vy2)
					tmp2.Mul(tmp2, x2)
					tmp1.Add(tmp1, tmp2)
					y.Quo(tmp1, dy)
					// y := (vy2*vx1*y1-vy1*vy2*x1-vy1*vx2*y2+vy1*vy2*x2)/d2
					if y.Cmp(minRat) < 0 || y.Cmp(maxRat) > 0 {
						continue
					}

					if vx1.Sign() != 0 {
						tmp1.Sub(x, x1)
						if tmp1.Sign()*vx1.Sign() < 0 {
							continue
						}
					} else {
						tmp1.Sub(y, y1)
						if tmp1.Sign()*vy1.Sign() < 0 {
							continue
						}
					}
					if vx2.Sign() != 0 {
						tmp1.Sub(x, x2)
						if tmp1.Sign()*vx2.Sign() < 0 {
							continue
						}
					} else {
						tmp1.Sub(y, y2)
						if tmp1.Sign()*vy2.Sign() < 0 {
							continue
						}
					}
					result++
				}
			}
			out <- result
		}()
	}
	for i := range hail {
		in <- i
	}
	close(in)
	result := 0
	for range runtime.NumCPU() {
		result += <-out
	}
	return IntResult(result)
}

func (aoc *aoc202324) Test1(input *Input) string {
	return aoc.part1(7, 27, input)
}

func (aoc *aoc202324) Part1(input *Input) string {
	return aoc.part1(200000000000000, 400000000000000, input)
}

func (aoc *aoc202324) Part2(input *Input) string {
	add := func(a, b *big.Rat) *big.Rat {
		return (&big.Rat{}).Add(a, b)
	}
	sub := func(a, b *big.Rat) *big.Rat {
		return (&big.Rat{}).Sub(a, b)
	}
	mul := func(a, b *big.Rat) *big.Rat {
		return (&big.Rat{}).Mul(a, b)
	}

	solve := func(mat [][]*big.Rat) {
		n := len(mat)
		// Upper-diagonalize
		for i := range n {
			for mat[i][i].Sign() == 0 {
				last := mat[i]
				copy(mat[i:], mat[i+1:])
				mat[n-1] = last
			}
			for k := i + 1; k < len(mat[i]); k++ {
				mat[i][k].Quo(mat[i][k], mat[i][i])
			}
			// mat[i][i].SetInt64(1)
			for j := i + 1; j < n; j++ {
				for k := i + 1; k < len(mat[j]); k++ {
					mat[j][k].Sub(mat[j][k], mul(mat[j][i], mat[i][k]))
				}
				// mat[j][i].SetInt64(0)
			}
		}
		// Back-substitute
		for i := n - 1; i >= 0; i-- {
			for j := range i {
				for k := n; k < len(mat[j]); k++ {
					mat[j][k].Sub(mat[j][k], mul(mat[j][i], mat[i][k]))
				}
				// mat[j][i].SetInt64(0)
			}
		}
	}

	hail := aoc.parse(5, input)
	x1, y1, z1, u1, v1, w1 := hail[0][0], hail[0][1], hail[0][2], hail[0][3], hail[0][4], hail[0][5]
	x2, y2, z2, u2, v2, w2 := hail[1][0], hail[1][1], hail[1][2], hail[1][3], hail[1][4], hail[1][5]
	x3, y3, z3, u3, v3, w3 := hail[2][0], hail[2][1], hail[2][2], hail[2][3], hail[2][4], hail[2][5]
	x4, y4, z4, u4, v4, w4 := hail[3][0], hail[3][1], hail[3][2], hail[3][3], hail[3][4], hail[3][5]
	x5, y5, z5, u5, v5, w5 := hail[4][0], hail[4][1], hail[4][2], hail[4][3], hail[4][4], hail[4][5]
	_, _, _, _ = z4, w4, z5, w5

	xuyvMat := [][]*big.Rat{
		[]*big.Rat{
			sub(v1, v2),
			sub(y2, y1),
			sub(u2, u1),
			sub(x1, x2),
			add(sub(mul(v1, x1), mul(v2, x2)),
				sub(mul(u2, y2), mul(u1, y1))),
		},
		[]*big.Rat{
			sub(v2, v3),
			sub(y3, y2),
			sub(u3, u2),
			sub(x2, x3),
			add(sub(mul(v2, x2), mul(v3, x3)),
				sub(mul(u3, y3), mul(u2, y2))),
		},
		[]*big.Rat{
			sub(v3, v4),
			sub(y4, y3),
			sub(u4, u3),
			sub(x3, x4),
			add(sub(mul(v3, x3), mul(v4, x4)),
				sub(mul(u4, y4), mul(u3, y3))),
		},
		[]*big.Rat{
			sub(v4, v5),
			sub(y5, y4),
			sub(u5, u4),
			sub(x4, x5),
			add(sub(mul(v4, x4), mul(v5, x5)),
				sub(mul(u5, y5), mul(u4, y4))),
		},
	}

	solve(xuyvMat)
	x := xuyvMat[0][4]
	u := xuyvMat[1][4]
	y := xuyvMat[2][4]

	zwMat := [][]*big.Rat{
		[]*big.Rat{
			sub(u2, u1),
			sub(x1, x2),
			add(
				add(
					sub(mul(w1, x1), mul(w2, x2)),
					sub(mul(u2, z2), mul(u1, z1)),
				),
				add(
					mul(sub(w2, w1), x),
					mul(sub(z1, z2), u),
				),
			),
		},
		[]*big.Rat{
			sub(u3, u2),
			sub(x2, x3),
			add(
				add(
					sub(mul(w2, x2), mul(w3, x3)),
					sub(mul(u3, z3), mul(u2, z2)),
				),
				add(
					mul(sub(w3, w2), x),
					mul(sub(z2, z3), u),
				),
			),
		},
	}
	solve(zwMat)
	z := zwMat[0][2]

	sum := add(x, y)
	sum.Add(sum, z)
	return sum.RatString()
}
