package main

func init() {
	Register(&aoc202207{
		AOC: AOC{
			Day:           7,
			InputFilename: "../2022/input/07.txt",
			Tests: []Test{
				Test{
					Input: `$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
`,
					Part1: "95437",
					Part2: "24933642",
				},
			},
		},
	})
}

type aoc202207 struct {
	AOC
}

type aoc202207_dir struct {
	parent *aoc202207_dir
	files  map[string]int
	dirs   map[string]*aoc202207_dir
	size   int
}

func (aoc *aoc202207) parse(input *Input) *aoc202207_dir {
	root := &aoc202207_dir{
		files: map[string]int{},
		dirs:  map[string]*aoc202207_dir{},
	}
	root.parent = root
	dir := root
	word, ok := input.Word()
	for {
		if word != "$" {
			panic("bad input")
		}
		word, _ = input.Word()
		switch word {
		case "cd":
			word, _ = input.Word()
			if word == ".." {
				dir = dir.parent
			} else if word == "/" {
				dir = root
			} else {
				dir = dir.dirs[word]
			}
			if dir == nil {
				panic("bad input")
			}
			word, ok = input.Word()
			if !ok {
				aoc.size(root)
				return root
			}
		case "ls":
			for {
				word, ok = input.Word()
				if !ok {
					aoc.size(root)
					return root
				}
				if word == "$" {
					break
				}
				if word == "dir" {
					word, _ = input.Word()
					dir.dirs[word] = &aoc202207_dir{
						parent: dir,
						files:  map[string]int{},
						dirs:   map[string]*aoc202207_dir{},
					}
				} else {
					size, _ := InputString(word).Int()
					word, _ = input.Word()
					dir.files[word] = size
				}
			}
		default:
			panic("bad input")
		}
	}
}

func (aoc *aoc202207) size(dir *aoc202207_dir) int {
	s := 0
	for _, d := range dir.dirs {
		s += aoc.size(d)
	}
	for _, fileSize := range dir.files {
		s += fileSize
	}
	dir.size = s
	return s
}

func (aoc *aoc202207) Part1(input *Input) string {
	root := aoc.parse(input)
	queue := NewQueue[*aoc202207_dir]()
	queue.Enqueue(root)

	result := 0
	for !queue.Empty() {
		dir := queue.Dequeue()
		if dir.size <= 100000 {
			result += dir.size
		}
		for _, d := range dir.dirs {
			queue.Enqueue(d)
		}
	}
	return IntResult(result)
}

func (aoc *aoc202207) Part2(input *Input) string {
	root := aoc.parse(input)
	queue := NewQueue[*aoc202207_dir]()
	queue.Enqueue(root)
	minSize := root.size - (70000000 - 30000000)

	result := root.size
	for !queue.Empty() {
		dir := queue.Dequeue()
		if dir.size < minSize {
			continue
		}
		result = min(result, dir.size)
		for _, d := range dir.dirs {
			queue.Enqueue(d)
		}
	}
	return IntResult(result)
}
