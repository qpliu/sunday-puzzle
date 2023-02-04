package main

import (
	"crypto/md5"
	"os"
)

type Path struct {
	x, y int
	path string
}

func search(passcode string) string {
	// breadth-first search
	paths := []Path{Path{0, 0, ""}}
	for {
		newpaths := []Path{}
		for _, path := range paths {
			hash := md5.Sum([]byte(passcode + path.path))
			if path.y > 0 && (hash[0]>>4) > 10 {
				newpaths = append(newpaths, Path{path.x, path.y - 1, path.path + "U"})
			}
			if path.y < 3 && (hash[0]&15) > 10 {
				if path.x == 3 && path.y == 2 {
					return path.path + "D"
				}
				newpaths = append(newpaths, Path{path.x, path.y + 1, path.path + "D"})
			}
			if path.x > 0 && (hash[1]>>4) > 10 {
				newpaths = append(newpaths, Path{path.x - 1, path.y, path.path + "L"})
			}
			if path.x < 3 && (hash[1]&15) > 10 {
				if path.x == 2 && path.y == 3 {
					return path.path + "R"
				}
				newpaths = append(newpaths, Path{path.x + 1, path.y, path.path + "R"})
			}
		}
		if len(newpaths) == 0 {
			panic("dead-ended:"+passcode)
		}
		paths = newpaths
	}
}

// depth-first search
func search2(passcode string, path Path) int {
	for {
		if path.x == 3 && path.y == 3 {
			return len(path.path)
		}
		newpaths := []Path{}
		hash := md5.Sum([]byte(passcode + path.path))
		if path.y > 0 && (hash[0]>>4) > 10 {
			newpaths = append(newpaths, Path{path.x, path.y - 1, path.path + "U"})
		}
		if path.y < 3 && (hash[0]&15) > 10 {
			newpaths = append(newpaths, Path{path.x, path.y + 1, path.path + "D"})
		}
		if path.x > 0 && (hash[1]>>4) > 10 {
			newpaths = append(newpaths, Path{path.x - 1, path.y, path.path + "L"})
		}
		if path.x < 3 && (hash[1]&15) > 10 {
			newpaths = append(newpaths, Path{path.x + 1, path.y, path.path + "R"})
		}
		if len(newpaths) == 0 {
			// dead-ended
			return 0
		}
		if len(newpaths) == 1 {
			path = newpaths[0]
			continue
		}
		longest := 0
		for _, newpath := range(newpaths) {
			l := search2(passcode, newpath)
			if l > longest {
				longest = l
			}
		}
		return longest
	}
}

func main() {
	if len(os.Args) < 2 {
		if p := search("ihgpwlah"); p != "DDRRRD" {
			panic(p)
		}
		if p := search("kglvqrro"); p != "DDUDRLRRUDRD" {
			panic(p)
		}
		if p := search("ulqzkmiv"); p != "DRURDRUDDLLDLUURRDULRLDUUDDDRR" {
			panic(p)
		}
		if p := search2("ihgpwlah", Path{0, 0, ""}); p != 370 {
			panic(p)
		}
		if p := search2("kglvqrro", Path{0, 0, ""}); p != 492 {
			panic(p)
		}
		if p := search2("ulqzkmiv", Path{0, 0, ""}); p != 830 {
			panic(p)
		}
	} else if len(os.Args) < 3 {
		println(search(os.Args[1]))
	} else {
		println(search2(os.Args[2], Path{0, 0, ""}))
	}
}
