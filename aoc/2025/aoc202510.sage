# -*- Python -*-

def parse_ints(str, i):
    ints = []
    n = 0
    while i < len(str):
        j = i
        while (str[j] == '0' or str[j] == '1' or str[j] == '2' or str[j] == '3'
               or str[j] == '4' or str[j] == '5' or str[j] == '6'
               or str[j] == '7' or str[j] == '8' or str[j] == '9'):
            j += 1
        if j == i:
            return ints, j
        else:
            ints.append(int(str[i:j]))
            if str[j] == ',':
                i = j+1
            else:
                return ints, j
    return ints, i

def parse_line(str):
    switches = []
    i = 0
    while i < len(str):
        if str[i] == '(':
            switch, i = parse_ints(str, i+1)
            switches.append(switch)
        elif str[i] == '{':
            goals, i = parse_ints(str, i+1)
            return switches, goals
        else:
            i += 1
    return switches, []

def test_input():
    return [
        "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
        "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
        "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    ]

def read_input():
    lines = []
    with open("input/10.txt") as file:
        for line in file:
            lines.append(line)
    return lines

def presses(str):
    switches, goals = parse_line(str)
    p = MixedIntegerLinearProgram()
    v = p.new_variable(integer=True, nonnegative=True)
    p.set_objective(-sum([v[i] for i in range(len(switches))]))
    for i in range(len(goals)):
        eq = 0
        for j in range(len(switches)):
            if i in switches[j]:
                eq += v[j]
        p.add_constraint(eq == goals[i])
    return -p.solve()

def test():
    return sum([presses(str) for str in test_input()])

def run():
    return sum([presses(str) for str in read_input()])

print(run())
