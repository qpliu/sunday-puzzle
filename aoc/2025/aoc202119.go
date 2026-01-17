package main

import (
	"runtime"
)

func init() {
	Register(&aoc202119{
		AOC: AOC{
			Day:           19,
			InputFilename: "../2021/input/19.txt",
			Tests: []Test{
				Test{
					Input: `--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14
`,
					Part1: "79",
					Part2: "3621",
				},
			},
		},
	})
}

type aoc202119 struct {
	AOC
}

func (aoc *aoc202119) parse(input *Input) [][][3]int {
	scanners := [][][3]int{}
	for paragraph := range input.Paragraphs() {
		in := InputString(paragraph)
		in.Int()
		scanner := [][3]int{}
		for {
			x, ok := in.Int()
			if !ok {
				break
			}
			y, _ := in.Int()
			z, _ := in.Int()
			scanner = append(scanner, [3]int{x, y, z})
		}
		scanners = append(scanners, scanner)
	}
	return scanners
}

// If two scanners have at least 12 beacons in common, they should have
// at least 66 pairs of beacons in common, which means if two scanners do
// not have at least 66 of these canonicalized distances in common, they
// don't need to be considered as overlapping.
//
// And, in the example and in my input, there are never more than 66 of
// these canonicalized distances in common for any two scanners.
// This cuts the number of pairs of pairs needed to determine the
// relative orientations and positions way down, since if there are
// pairs with these canonicalized distances that match, at least one of
// the pairs from one of the scanners is guaranteed to match one of the
// pairs from the other scanner.  This alone should make this solution
// much faster than my previous solutions.
func (aoc *aoc202119) cdists(beacons [][3]int) map[[3]int][][2][3]int {
	cdists := map[[3]int][][2][3]int{}
	for i, xyz1 := range beacons {
		for _, xyz2 := range beacons[:i] {
			d0 := max(xyz1[0]-xyz2[0], xyz2[0]-xyz1[0])
			d1 := max(xyz1[1]-xyz2[1], xyz2[1]-xyz1[1])
			d2 := max(xyz1[2]-xyz2[2], xyz2[2]-xyz1[2])
			if d1 < d0 {
				d0, d1 = d1, d0
			}
			if d2 < d1 {
				d1, d2 = d2, d1
			}
			if d1 < d0 {
				d0, d1 = d1, d0
			}
			cdist := [3]int{d0, d1, d2}
			cdists[cdist] = append(cdists[cdist], [2][3]int{xyz1, xyz2})
		}
	}
	return cdists
}

func (aoc *aoc202119) mapBeacon(orientation, dx, dy, dz int, xyz [3]int) [3]int {
	switch orientation {
	case 0:
		return [3]int{xyz[0] + dx, xyz[1] + dy, xyz[2] + dz}
	case 1:
		return [3]int{xyz[0] + dx, -xyz[2] + dy, xyz[1] + dz}
	case 2:
		return [3]int{xyz[0] + dx, -xyz[1] + dy, -xyz[2] + dz}
	case 3:
		return [3]int{xyz[0] + dx, xyz[2] + dy, -xyz[1] + dz}
	case 4:
		return [3]int{-xyz[0] + dx, -xyz[1] + dy, xyz[2] + dz}
	case 5:
		return [3]int{-xyz[0] + dx, -xyz[2] + dy, -xyz[1] + dz}
	case 6:
		return [3]int{-xyz[0] + dx, xyz[1] + dy, -xyz[2] + dz}
	case 7:
		return [3]int{-xyz[0] + dx, xyz[2] + dy, xyz[1] + dz}
	case 8:
		return [3]int{xyz[1] + dx, -xyz[0] + dy, xyz[2] + dz}
	case 9:
		return [3]int{xyz[1] + dx, -xyz[2] + dy, -xyz[0] + dz}
	case 10:
		return [3]int{xyz[1] + dx, xyz[0] + dy, -xyz[2] + dz}
	case 11:
		return [3]int{xyz[1] + dx, xyz[2] + dy, xyz[0] + dz}
	case 12:
		return [3]int{-xyz[1] + dx, xyz[0] + dy, xyz[2] + dz}
	case 13:
		return [3]int{-xyz[1] + dx, -xyz[2] + dy, xyz[0] + dz}
	case 14:
		return [3]int{-xyz[1] + dx, -xyz[0] + dy, -xyz[2] + dz}
	case 15:
		return [3]int{-xyz[1] + dx, xyz[2] + dy, -xyz[0] + dz}
	case 16:
		return [3]int{xyz[2] + dx, xyz[1] + dy, -xyz[0] + dz}
	case 17:
		return [3]int{xyz[2] + dx, xyz[0] + dy, xyz[1] + dz}
	case 18:
		return [3]int{xyz[2] + dx, -xyz[1] + dy, xyz[0] + dz}
	case 19:
		return [3]int{xyz[2] + dx, -xyz[0] + dy, -xyz[1] + dz}
	case 20:
		return [3]int{-xyz[2] + dx, xyz[1] + dy, xyz[0] + dz}
	case 21:
		return [3]int{-xyz[2] + dx, -xyz[0] + dy, xyz[1] + dz}
	case 22:
		return [3]int{-xyz[2] + dx, -xyz[1] + dy, -xyz[0] + dz}
	case 23:
		return [3]int{-xyz[2] + dx, xyz[0] + dy, -xyz[1] + dz}
	default:
		panic("?")
	}
}

func (aoc *aoc202119) mapScanner(scanner [][3]int, cdist map[[3]int][][2][3]int, beacons map[[3]int]bool, refScanner [][3]int, refCdist map[[3]int][][2][3]int, refBeacons map[[3]int]bool) ([3]int, bool) {
	for cd, pairs := range cdist {
		for _, refPair := range refCdist[cd] {
			rxyz1 := refPair[0]
			rxyz2 := refPair[1]
			for _, pair := range pairs {
				for orientation := range 24 {
					xyz1 := aoc.mapBeacon(orientation, 0, 0, 0, pair[0])
					xyz2 := aoc.mapBeacon(orientation, 0, 0, 0, pair[1])
					if xyz1[0]-xyz2[0] != rxyz1[0]-rxyz2[0] || xyz1[1]-xyz2[1] != rxyz1[1]-rxyz2[1] || xyz1[2]-xyz2[2] != rxyz1[2]-rxyz2[2] {
						continue
					}
					dx := rxyz1[0] - xyz1[0]
					dy := rxyz1[1] - xyz1[1]
					dz := rxyz1[2] - xyz1[2]
					common := 0
					for _, b := range scanner {
						if refBeacons[aoc.mapBeacon(orientation, dx, dy, dz, b)] {
							common++
						}
					}
					if common < 12 {
						continue
					}
					for i, b := range scanner {
						scanner[i] = aoc.mapBeacon(orientation, dx, dy, dz, b)
					}
					clear(beacons)
					for _, b := range scanner {
						beacons[b] = true
					}
					for _, cdpairs := range cdist {
						for i := range cdpairs {
							cdpairs[i][0] = aoc.mapBeacon(orientation, dx, dy, dz, cdpairs[i][0])
							cdpairs[i][1] = aoc.mapBeacon(orientation, dx, dy, dz, cdpairs[i][1])
						}
					}
					return [3]int{dx, dy, dz}, true
				}
			}
		}
	}
	return [3]int{}, false
}

func (aoc *aoc202119) mapBeacons(input *Input) (map[[3]int]bool, [][3]int) {
	scanners := aoc.parse(input)
	cdists := []map[[3]int][][2][3]int{}
	beacons := []map[[3]int]bool{}
	for _, scanner := range scanners {
		cdists = append(cdists, aoc.cdists(scanner))
		b := map[[3]int]bool{}
		for _, xyz := range scanner {
			b[xyz] = true
		}
		beacons = append(beacons, b)
	}
	overlaps := map[int][]int{}
	for i, cdists1 := range cdists {
		for j, cdists2 := range cdists {
			if i == j {
				continue
			}
			common := 0
			for dist, pairs := range cdists1 {
				common += min(len(pairs), len(cdists2[dist]))
			}
			if common >= 66 {
				overlaps[i] = append(overlaps[i], j)
				overlaps[j] = append(overlaps[j], i)
			}
		}
	}

	mapped := make([]bool, len(scanners))
	scannerLocs := make([][3]int, len(scanners))
	mapped[0] = true
	in := make(chan int)
	out := make(chan int)
	for range runtime.NumCPU() {
		go func() {
			for i := range in {
				if mapped[i] {
					out <- i
					continue
				}
				for _, j := range overlaps[i] {
					if !mapped[j] {
						continue
					}
					scannerLoc, ok := aoc.mapScanner(scanners[i], cdists[i], beacons[i], scanners[j], cdists[j], beacons[j])
					if ok {
						mapped[i] = true
						scannerLocs[i] = scannerLoc
						break
					}
				}
				out <- i
			}
		}()
	}

	queue := NewQueue[int]()
	for i := range scanners {
		queue.Enqueue(i)
	}
	unmapped := len(scanners)
	for unmapped > 0 {
		if queue.Empty() {
			i := <-out
			if mapped[i] {
				unmapped--
			} else {
				queue.Enqueue(i)
			}
		} else {
			i := queue.Dequeue()
			select {
			case in <- i:
			case j := <-out:
				queue.Enqueue(i)
				if mapped[j] {
					unmapped--
				} else {
					queue.Enqueue(j)
				}
			}
		}
	}
	close(in)

	allBeacons := map[[3]int]bool{}
	for _, scanner := range scanners {
		for _, beacon := range scanner {
			allBeacons[beacon] = true
		}
	}
	return allBeacons, scannerLocs
}

func (aoc *aoc202119) Part1(input *Input) string {
	beacons, _ := aoc.mapBeacons(input)
	return IntResult(len(beacons))
}

func (aoc *aoc202119) Part2(input *Input) string {
	_, scanners := aoc.mapBeacons(input)
	result := 0
	for i, xyz1 := range scanners {
		for _, xyz2 := range scanners[:i] {
			result = max(result, max(xyz1[0]-xyz2[0], xyz2[0]-xyz1[0])+max(xyz1[1]-xyz2[1], xyz2[1]-xyz1[1])+max(xyz1[2]-xyz2[2], xyz2[2]-xyz1[2]))
		}
	}
	return IntResult(result)
}
