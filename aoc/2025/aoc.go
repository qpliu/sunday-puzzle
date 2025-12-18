package main

import (
	"container/heap"
	"fmt"
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
	if in.index+len(str) > len(in.data) {
		return false
	}
	for i := range str {
		if str[i] != in.data[i+in.index] {
			return false
		}
	}
	in.index += len(str)
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

type PriorityQueue[T interface{ Priority() int }] struct {
	pq priorityQueueArray[T]
}

func (pq *PriorityQueue[T]) Push(item T) {
	heap.Push(&pq.pq, &priorityQueueItem[T]{
		value:    item,
		priority: item.Priority(),
	})
}

func (pq *PriorityQueue[T]) Pop() (T, bool) {
	if len(pq.pq) == 0 {
		var zero T
		return zero, false
	}
	return heap.Pop(&pq.pq).(*priorityQueueItem[T]).value, true
}

type AstarPath[ST comparable] interface {
	Done() bool
	Priority() int
	State() ST
	Neighbors() []AstarPath[ST]
}

func AstarSearch[ST comparable](start []AstarPath[ST]) AstarPath[ST] {
	open := PriorityQueue[AstarPath[ST]]{}
	for _, item := range start {
		open.Push(item)
	}
	visited := map[ST]int{}
	for {
		path, ok := open.Pop()
		if !ok {
			return nil
		}
		if path.Done() {
			return path
		}
		state := path.State()
		priority := path.Priority()
		visitedPriority, ok := visited[state]
		if ok && priority <= visitedPriority {
			continue
		}
		visited[state] = priority
		for _, neighbor := range path.Neighbors() {
			open.Push(neighbor)
		}
	}
}

func AstarSearchAll[ST comparable](start []AstarPath[ST]) []AstarPath[ST] {
	open := PriorityQueue[AstarPath[ST]]{}
	for _, item := range start {
		open.Push(item)
	}
	visited := map[ST]int{}
	done := []AstarPath[ST]{}
	donePriority := 0
	for {
		path, ok := open.Pop()
		if !ok {
			return done
		}
		priority := path.Priority()
		if len(done) > 0 {
			if priority < donePriority {
				return done
			}
		}
		if path.Done() {
			if len(done) == 0 {
				donePriority = priority
			}
			done = append(done, path)
			continue
		}
		state := path.State()
		visitedPriority, ok := visited[state]
		if ok && priority < visitedPriority {
			continue
		}
		visited[state] = priority
		for _, neighbor := range path.Neighbors() {
			open.Push(neighbor)
		}
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
