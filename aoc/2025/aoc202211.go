package main

func init() {
	Register(&aoc202211{
		AOC: AOC{
			Day:           11,
			InputFilename: "../2022/input/11.txt",
			Tests: []Test{
				Test{
					Input: `Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
`,
					Part1: "10605",
					Part2: "2713310158",
				},
			},
		},
	})
}

type aoc202211 struct {
	AOC
}

type aoc202211_monkey struct {
	items *Queue[int]
	op    func(int) int
	test  int
	t, f  int
	count int
}

func (aoc *aoc202211) parse(input *Input) (int, []*aoc202211_monkey) {
	monkeys := []*aoc202211_monkey{}
	limit := 1
	for {
		n, ok := input.Int()
		if !ok {
			return limit, monkeys
		}
		if n != len(monkeys) {
			panic("bad input")
		}
		input.Word()
		input.Word()
		line, _ := input.Line()
		items := InputString(line).Ints()
		input.Skip("  Operation: new = old ")
		op, _ := input.Word()
		arg, _ := input.Word()
		test, _ := input.Int()
		t, _ := input.Int()
		f, _ := input.Int()
		queue := NewQueue[int]()
		for _, item := range items {
			queue.Enqueue(item)
		}
		var operation func(int) int
		if op == "*" {
			if arg == "old" {
				operation = func(n int) int {
					return n * n
				}
			} else {
				a, _ := InputString(arg).Int()
				operation = func(n int) int {
					return n * a
				}
			}
		} else if op == "+" {
			a, _ := InputString(arg).Int()
			operation = func(n int) int {
				return n + a
			}
		} else {
			panic("bad input")
		}
		limit = LCM(limit, test)
		monkeys = append(monkeys, &aoc202211_monkey{
			items: queue,
			op:    operation,
			test:  test,
			t:     t,
			f:     f,
		})
	}
}

func (aoc *aoc202211) result(rounds, divisor int, input *Input) string {
	limit, monkeys := aoc.parse(input)
	for range rounds {
		for _, m := range monkeys {
			m.count += m.items.Len()
			for !m.items.Empty() {
				item := m.items.Dequeue()
				item = (m.op(item) / divisor) % limit
				if item%m.test == 0 {
					monkeys[m.t].items.Enqueue(item)
				} else {
					monkeys[m.f].items.Enqueue(item)
				}
			}
		}
	}
	count1 := 0
	count2 := 0
	for _, m := range monkeys {
		c := m.count
		if c > count1 {
			count1, c = c, count1
		}
		if c > count2 {
			count2 = c
		}
	}
	return IntResult(count1 * count2)
}

func (aoc *aoc202211) Part1(input *Input) string {
	return aoc.result(20, 3, input)
}

func (aoc *aoc202211) Part2(input *Input) string {
	return aoc.result(10000, 1, input)
}
