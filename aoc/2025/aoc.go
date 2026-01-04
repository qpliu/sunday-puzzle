package main

import (
	"bytes"
	"container/heap"
	"container/list"
	"fmt"
	"iter"
	"math/bits"
	"os"
	"time"
	"unicode"
)

type AOC struct {
	Day           int
	Tests         []Test
	InputFilename string // "" defaults to "input/day.txt"
}

func (aoc AOC) GetAOC() AOC {
	return aoc
}

type Test struct {
	Input string
	Part1 string
	Part2 string
}

type AOCDay interface {
	GetAOC() AOC
	Part1(*Input) string
	Part2(*Input) string
}

type AOCReset interface {
	GetAOC() AOC
	Reset()
}

type AOCTest1 interface {
	GetAOC() AOC
	Test1(*Input) string
}

type AOCTest2 interface {
	GetAOC() AOC
	Test2(*Input) string
}

var aocDays []AOCDay

func Register(aocDay AOCDay) {
	aocDays = append(aocDays, aocDay)
}

type Input struct {
	data  string
	index int
}

func InputString(data string) *Input {
	return &Input{data: data}
}

func InputFile(aoc AOC) *Input {
	filename := aoc.InputFilename
	if filename == "" {
		filename = fmt.Sprintf("input/%02d.txt", aoc.Day)
	}
	if bytes, err := os.ReadFile(filename); err != nil {
		panic(err)
	} else {
		return &Input{data: string(bytes)}
	}
}

func (in *Input) Reset() {
	in.index = 0
}

func (in *Input) Word() (string, bool) {
	for in.index < len(in.data) && unicode.IsSpace(rune(in.data[in.index])) {
		in.index++
	}
	if in.index >= len(in.data) {
		return "", false
	}
	istart := in.index
	for in.index < len(in.data) && !unicode.IsSpace(rune(in.data[in.index])) {
		in.index++
	}
	return in.data[istart:in.index], true
}

type Seq[T any] = iter.Seq[T]
type Seq2[T1 any, T2 any] = iter.Seq2[T1, T2]

func (in *Input) Words() Seq[string] {
	return func(yield func(string) bool) {
		for word, ok := in.Word(); ok; word, ok = in.Word() {
			if !yield(word) {
				return
			}
		}
	}
}

func (in *Input) Line() (string, bool) {
	if in.index >= len(in.data) {
		return "", false
	}
	istart := in.index
	for in.index < len(in.data) && in.data[in.index] != '\n' {
		in.index++
	}
	iend := in.index
	in.index++
	return in.data[istart:iend], true
}

func (in *Input) Lines() Seq[string] {
	return func(yield func(string) bool) {
		for line, ok := in.Line(); ok; line, ok = in.Line() {
			if !yield(line) {
				return
			}
		}
	}
}

func (in *Input) Paragraph() (string, bool) {
	if in.index >= len(in.data) {
		return "", false
	}
	istart := in.index
	for in.index < len(in.data)-1 && !(in.data[in.index] == '\n' && in.data[in.index+1] == '\n') {
		in.index++
	}
	iend := in.index
	in.index += 2
	return in.data[istart:iend], true
}

func (in *Input) Paragraphs() Seq[string] {
	return func(yield func(string) bool) {
		for paragraph, ok := in.Paragraph(); ok; paragraph, ok = in.Paragraph() {
			if !yield(paragraph) {
				return
			}
		}
	}
}

func (in *Input) Int() (int, bool) {
	for {
		for in.index < len(in.data) && in.data[in.index] != '-' && !unicode.IsDigit(rune(in.data[in.index])) {
			in.index++
		}
		if in.index >= len(in.data) {
			return 0, false
		}
		negative := false
		if in.index < len(in.data) && in.data[in.index] == '-' {
			negative = true
			in.index++
			if in.index >= len(in.data) {
				return 0, false
			}
			if !unicode.IsDigit(rune(in.data[in.index])) {
				continue
			}
		}
		i := 0
		for in.index < len(in.data) && unicode.IsDigit(rune(in.data[in.index])) {
			i *= 10
			i += int(in.data[in.index]) - '0'
			in.index++
		}
		if negative {
			i = -i
		}
		return i, true
	}
}

func (in *Input) Ints() []int {
	var ints []int
	for i, ok := in.Int(); ok; i, ok = in.Int() {
		ints = append(ints, i)
	}
	return ints
}

func (in *Input) Char() (byte, bool) {
	if in.index >= len(in.data) {
		return 0, false
	}
	in.index++
	return in.data[in.index-1], true
}

func (in *Input) Chars() Seq[byte] {
	return func(yield func(byte) bool) {
		for ch, ok := in.Char(); ok; ch, ok = in.Char() {
			if !yield(ch) {
				return
			}
		}
	}
}

func (in *Input) Peek() (byte, bool) {
	if in.index >= len(in.data) {
		return 0, false
	}
	return in.data[in.index], true
}

func (in *Input) Grid() (int, int, map[[2]int]byte) {
	x := 0
	y := 0
	xmax := -1
	ymax := -1
	grid := map[[2]int]byte{}
	for in.index < len(in.data) {
		if in.data[in.index] == '\n' {
			x = 0
			y++
		} else {
			grid[[2]int{x, y}] = in.data[in.index]
			xmax = max(x, xmax)
			ymax = max(y, ymax)
			x++
		}
		in.index++
	}
	return xmax + 1, ymax + 1, grid
}

func (in *Input) Skip(str string) bool {
	if in.LookingAt(str) {
		in.index += len(str)
		return true
	}
	return false
}

func (in *Input) LookingAt(str string) bool {
	if in.index+len(str) > len(in.data) {
		return false
	}
	for i := range str {
		if str[i] != in.data[i+in.index] {
			return false
		}
	}
	return true
}

func (in *Input) All() string {
	return in.data[in.index:]
}

func GCD(x, y int) int {
	for y != 0 {
		x, y = y, MOD(x, y)
	}
	return x
}

func LCM(x, y int) int {
	return x * y / GCD(x, y)
}

func MOD(x, y int) int {
	if y < 0 {
		return -((-x)%(-y) - y) % (-y)
	} else {
		return (x%y + y) % y
	}
}

func EGCD(a, b int) (int, int) {
	if a == 0 {
		return 0, 1
	}
	x, y := EGCD(MOD(b, a), a)
	return y - (b/a)*x, x
}

func Convergences(x, y [2]int) [2]int {
	offset := x[0]
	recurX := x[1]
	firstY := y[0] - x[0]
	recurY := y[1]
	if firstY < 0 {
		offset = y[0]
		recurX = y[1]
		firstY = x[0] - y[0]
		recurY = x[1]
	}
	rY := recurY / GCD(recurX, recurY)
	nx := 0
	{
		m, _ := EGCD(recurX, rY)
		nx = MOD(MOD(m, rY)*firstY, rY)
	}
	recurXY := LCM(recurX, recurY)
	nrecur := max(0, firstY-nx*recurX) / recurXY
	if firstY%recurX == 0 {
		return [2]int{offset + firstY, recurXY}
	} else {
		return [2]int{offset + nx*recurX + nrecur*recurXY, recurXY}
	}
}

type priorityQueueItem[T any] struct {
	value    T
	priority int
}

type priorityQueueArray[T any] []*priorityQueueItem[T]

func (pq priorityQueueArray[T]) Len() int {
	return len(pq)
}

func (pq priorityQueueArray[T]) Swap(i, j int) {
	pq[i], pq[j] = pq[j], pq[i]
}

func (pq priorityQueueArray[T]) Less(i, j int) bool {
	return pq[i].priority > pq[j].priority
}

func (pq *priorityQueueArray[T]) Push(value any) {
	item := value.(*priorityQueueItem[T])
	*pq = append(*pq, item)
}

func (pq *priorityQueueArray[T]) Pop() any {
	i := len(*pq) - 1
	item := (*pq)[i]
	(*pq)[i] = nil
	*pq = (*pq)[:i]
	return item
}

type PriorityQueue[T any] struct {
	pq       priorityQueueArray[T]
	priority func(T) int
}

func NewPriorityQueue[T any](priority func(T) int) PriorityQueue[T] {
	return PriorityQueue[T]{priority: priority}
}

func (pq *PriorityQueue[T]) Push(item T) {
	heap.Push(&pq.pq, &priorityQueueItem[T]{
		value:    item,
		priority: pq.priority(item),
	})
}

func (pq *PriorityQueue[T]) Pop() (T, bool) {
	if len(pq.pq) == 0 {
		var zero T
		return zero, false
	}
	return heap.Pop(&pq.pq).(*priorityQueueItem[T]).value, true
}

func AstarSearch[PATH any, ST comparable](start []PATH, done func(PATH) bool, priority func(PATH) int, state func(PATH) ST, neighbors func(PATH) []PATH) (PATH, bool) {
	open := NewPriorityQueue[PATH](priority)
	for _, item := range start {
		open.Push(item)
	}
	visited := map[ST]int{}
	for {
		path, ok := open.Pop()
		if !ok {
			return path, false
		}
		if done(path) {
			return path, true
		}
		st := state(path)
		prior := priority(path)
		visitedPriority, ok := visited[st]
		if ok && prior <= visitedPriority {
			continue
		}
		visited[st] = prior
		for _, neighbor := range neighbors(path) {
			open.Push(neighbor)
		}
	}
}

func AstarSearchAll[PATH any, ST comparable](start []PATH, done func(PATH) bool, priority func(PATH) int, state func(PATH) ST, neighbors func(PATH) []PATH) []PATH {
	open := NewPriorityQueue[PATH](priority)
	for _, item := range start {
		open.Push(item)
	}
	visited := map[ST]int{}
	donePaths := []PATH{}
	donePriority := 0
	for {
		path, ok := open.Pop()
		if !ok {
			return donePaths
		}
		prior := priority(path)
		if len(donePaths) > 0 {
			if prior < donePriority {
				return donePaths
			}
		}
		if done(path) {
			if len(donePaths) == 0 {
				donePriority = prior
			}
			donePaths = append(donePaths, path)
			continue
		}
		st := state(path)
		visitedPriority, ok := visited[st]
		if ok && prior < visitedPriority {
			continue
		}
		visited[st] = prior
		for _, neighbor := range neighbors(path) {
			open.Push(neighbor)
		}
	}
}

const (
	DirR = 1 << iota
	DirD
	DirL
	DirU
)

type XY = [2]int
type XYDir = [3]int

func AdvanceXYDir(xyDir XYDir, count int) XYDir {
	switch xyDir[2] {
	case DirR:
		xyDir[0] += count
	case DirD:
		xyDir[1] += count
	case DirL:
		xyDir[0] -= count
	case DirU:
		xyDir[1] -= count
	default:
		panic("?")
	}
	return xyDir
}

func AdvanceXY(xy XY, dir, count int) XY {
	switch dir {
	case DirR:
		xy[0] += count
	case DirD:
		xy[1] += count
	case DirL:
		xy[0] -= count
	case DirU:
		xy[1] -= count
	case DirR | DirD:
		xy[0] += count
		xy[1] += count
	case DirL | DirD:
		xy[0] -= count
		xy[1] += count
	case DirL | DirU:
		xy[0] -= count
		xy[1] -= count
	case DirR | DirU:
		xy[0] += count
		xy[1] -= count
	case 0:
	default:
		panic("?")
	}
	return xy
}

func ToXY(xyDir XYDir) XY {
	return [2]int{xyDir[0], xyDir[1]}
}

func ToXYDir(xy XY, dir int) XYDir {
	return [3]int{xy[0], xy[1], dir}
}

func TurnL(dir int) int {
	switch dir {
	case DirR:
		return DirU
	case DirD:
		return DirR
	case DirL:
		return DirD
	case DirU:
		return DirL
	default:
		panic("?")
	}
}

func TurnR(dir int) int {
	switch dir {
	case DirR:
		return DirD
	case DirD:
		return DirL
	case DirL:
		return DirU
	case DirU:
		return DirR
	default:
		panic("?")
	}
}

func XYDirTurnL(xyDir XYDir) XYDir {
	xyDir[2] = TurnL(xyDir[2])
	return xyDir
}

func XYDirTurnR(xyDir XYDir) XYDir {
	xyDir[2] = TurnR(xyDir[2])
	return xyDir
}

type Queue[T any] list.List

func NewQueue[T any]() *Queue[T] {
	return (*Queue[T])(list.New())
}

func (q *Queue[T]) Empty() bool {
	return ((*list.List)(q)).Len() == 0
}

func (q *Queue[T]) Len() int {
	return ((*list.List)(q)).Len()
}

func (q *Queue[T]) Enqueue(item T) {
	((*list.List)(q)).PushFront(item)
}

func (q *Queue[T]) Dequeue() T {
	return ((*list.List)(q)).Remove(((*list.List)(q)).Back()).(T)
}

// Can't do: type BitSet[T [...]uint64] T
// Can't do: type BitSet[T [...]uint64] struct { bs T }
// but perhaps a future version of Go will support it.
// Could do:
//  type BitSet[T interface{[1]uint64 | [2]uint64 | [4]uint64}] struct { bs T }
// but then, cannot range over bs.bs.
// Don't want to use reflection, since this has to be fast.

type BitSet64 uint64

func (bs BitSet64) Len() int {
	return bits.OnesCount64(uint64(bs))
}

func (bs BitSet64) Empty() bool {
	return bs.Len() == 0
}

func (bs BitSet64) Add(i int) BitSet64 {
	bs |= BitSet64(1 << i)
	return bs
}

func (bs BitSet64) Remove(i int) BitSet64 {
	bs &^= BitSet64(1 << i)
	return bs
}

func (bs BitSet64) Contains(i int) bool {
	return bs&BitSet64(1<<i) != 0
}

type BitSet128 [2]uint64

func (bs BitSet128) Len() int {
	return bits.OnesCount64(bs[0]) + bits.OnesCount64(bs[1])
}

func (bs BitSet128) Empty() bool {
	return bs.Len() == 0
}

func (bs BitSet128) Add(i int) BitSet128 {
	bs[i/64] |= uint64(1 << (i % 64))
	return bs
}

func (bs BitSet128) Remove(i int) BitSet128 {
	bs[i/64] &^= uint64(1 << (i % 64))
	return bs
}

func (bs BitSet128) Contains(i int) bool {
	return bs[i/64]&uint64(1<<(i%64)) != 0
}

type BitSet256 [4]uint64

func (bs BitSet256) Len() int {
	return bits.OnesCount64(bs[0]) + bits.OnesCount64(bs[1]) + bits.OnesCount64(bs[2]) + bits.OnesCount64(bs[3])
}

func (bs BitSet256) Empty() bool {
	return bs.Len() == 0
}

func (bs BitSet256) Add(i int) BitSet256 {
	bs[i/64] |= uint64(1 << (i % 64))
	return bs
}

func (bs BitSet256) Remove(i int) BitSet256 {
	bs[i/64] &^= uint64(1 << (i % 64))
	return bs
}

func (bs BitSet256) Contains(i int) bool {
	return bs[i/64]&uint64(1<<(i%64)) != 0
}

func OCR4x6(pixels []string) string {
	chars := map[[6]string]byte{
		[6]string{
			".##.",
			"#..#",
			"#..#",
			"####",
			"#..#",
			"#..#",
		}: 'A',
		[6]string{
			"###.",
			"#..#",
			"###.",
			"#..#",
			"#..#",
			"###.",
		}: 'B',
		[6]string{
			".##.",
			"#..#",
			"#...",
			"#...",
			"#..#",
			".##.",
		}: 'C',
		[6]string{
			"###.",
			"#..#",
			"#..#",
			"#..#",
			"#..#",
			"###.",
		}: 'D',
		[6]string{
			"####",
			"#...",
			"####",
			"#...",
			"#...",
			"####",
		}: 'E',
		[6]string{
			"####",
			"#...",
			"###.",
			"#...",
			"#...",
			"#...",
		}: 'F',
		[6]string{
			".##.",
			"#..#",
			"#...",
			"#.##",
			"#..#",
			".###",
		}: 'G',
		[6]string{
			"#..#",
			"#..#",
			"####",
			"#..#",
			"#..#",
			"#..#",
		}: 'H',
		[6]string{
			"###.",
			".#..",
			".#..",
			".#..",
			".#..",
			"###.",
		}: 'I',
		[6]string{
			"..##",
			"...#",
			"...#",
			"...#",
			"#..#",
			".##.",
		}: 'J',
		[6]string{
			"#..#",
			"#.#.",
			"##..",
			"#.#.",
			"#..#",
			"#..#",
		}: 'K',
		[6]string{
			"#...",
			"#...",
			"#...",
			"#...",
			"#...",
			"####",
		}: 'L',
		[6]string{
			"#..#",
			"####",
			"#..#",
			"#..#",
			"#..#",
			"#..#",
		}: 'M',
		[6]string{
			"#..#",
			"##.#",
			"##.#",
			"#.##",
			"#.##",
			"#..#",
		}: 'N',
		[6]string{
			".##.",
			"#..#",
			"#..#",
			"#..#",
			"#..#",
			".##.",
		}: 'O',
		[6]string{
			"###.",
			"#..#",
			"#..#",
			"###.",
			"#...",
			"#...",
		}: 'P',
		[6]string{
			".##.",
			"#..#",
			"#..#",
			"#..#",
			"#.##",
			".###",
		}: 'Q',
		[6]string{
			"###.",
			"#..#",
			"#..#",
			"###.",
			"#.#.",
			"#..#",
		}: 'R',
		[6]string{
			".##.",
			"#..#",
			".#..",
			"..#.",
			"#..#",
			".##.",
		}: 'S',
		[6]string{
			"###.",
			".#..",
			".#..",
			".#..",
			".#..",
			".#..",
		}: 'T',
		[6]string{
			"#..#",
			"#..#",
			"#..#",
			"#..#",
			"#..#",
			".##.",
		}: 'U',
		[6]string{
			"#..#",
			"#..#",
			"#..#",
			".##.",
			".##.",
			".##.",
		}: 'V',
		[6]string{
			"#..#",
			"#..#",
			"#..#",
			"#..#",
			".##.",
			".##.",
		}: 'W',
		[6]string{
			"#..#",
			"#..#",
			".##.",
			".##.",
			"#..#",
			"#..#",
		}: 'X',
		[6]string{
			"#.#.",
			"#.#.",
			"#.#.",
			".#..",
			".#..",
			".#..",
		}: 'Y',
		[6]string{
			"####",
			"...#",
			"..#.",
			".#..",
			"#...",
			"####",
		}: 'Z',
	}

	if len(pixels) < 6 {
		return ""
	}

	buf := &bytes.Buffer{}
	i := 0
	char := [6]string{}
	for {
		if i+4 > len(pixels[0]) {
			return buf.String()
		}
		for r := range 6 {
			char[r] = pixels[r][i : i+4]
		}
		buf.WriteByte(chars[char])
		i += 5
	}
}

func IntResult(result int) string {
	return fmt.Sprintf("%d", result)
}

func runTest1(aocDay AOCDay) bool {
	fail := false
	aoc := aocDay.GetAOC()
	for _, test := range aoc.Tests {
		if test.Part1 == "" {
			continue
		}
		if aocReset, ok := aocDay.(AOCReset); ok {
			aocReset.Reset()
		}
		result := ""
		if aocTest, ok := aocDay.(AOCTest1); ok {
			result = aocTest.Test1(InputString(test.Input))
		} else {
			result = aocDay.Part1(InputString(test.Input))
		}
		if result != test.Part1 {
			fail = true
			fmt.Printf("Day %02d part 1 test failed: %s != %s\n", aoc.Day, result, test.Part1)
		}
	}
	return !fail
}

func runTest2(aocDay AOCDay) bool {
	fail := false
	aoc := aocDay.GetAOC()
	for _, test := range aoc.Tests {
		if test.Part2 == "" {
			continue
		}
		if aocReset, ok := aocDay.(AOCReset); ok {
			aocReset.Reset()
		}
		result := ""
		if aocTest, ok := aocDay.(AOCTest2); ok {
			result = aocTest.Test2(InputString(test.Input))
		} else {
			result = aocDay.Part2(InputString(test.Input))
		}
		if result != test.Part2 {
			fail = true
			fmt.Printf("Day %02d part 2 test failed: %s != %s\n", aoc.Day, result, test.Part2)
		}
	}
	return !fail
}

func runPart1(aocDay AOCDay) time.Duration {
	aoc := aocDay.GetAOC()
	if aocReset, ok := aocDay.(AOCReset); ok {
		aocReset.Reset()
	}
	in := InputFile(aoc)
	start := time.Now()
	result := aocDay.Part1(in)
	duration := time.Since(start)
	fmt.Printf("Day %02d part 1: %s\n", aoc.Day, result)
	fmt.Printf("Day %02d part 1 time: %s\n", aoc.Day, duration)
	return duration
}

func runPart2(aocDay AOCDay) time.Duration {
	aoc := aocDay.GetAOC()
	if aocReset, ok := aocDay.(AOCReset); ok {
		aocReset.Reset()
	}
	in := InputFile(aoc)
	start := time.Now()
	result := aocDay.Part2(in)
	duration := time.Since(start)
	fmt.Printf("Day %02d part 2: %s\n", aoc.Day, result)
	fmt.Printf("Day %02d part 2 time: %s\n", aoc.Day, duration)
	return duration
}

func runDay(aocDay AOCDay) time.Duration {
	aoc := aocDay.GetAOC()
	totalTime := time.Duration(0)
	if runTest1(aocDay) {
		totalTime += runPart1(aocDay)
	}
	if runTest2(aocDay) {
		totalTime += runPart2(aocDay)
	}
	fmt.Printf("Day %02d total time: %s\n", aoc.Day, totalTime)
	return totalTime
}

func main() {
	totalTime := time.Duration(0)
	nDays := 0
	for _, aocDay := range aocDays {
		totalTime += runDay(aocDay)
		nDays++
	}
	if nDays > 1 {
		fmt.Printf("Total time: %s\n", totalTime)
	}
}
