/*
--- Day 17: Two Steps Forward ---

You're trying to access a secure vault protected by a 4x4 grid of small rooms
connected by doors. You start in the top-left room (marked S), and you can
access the vault (marked V) once you reach the bottom-right room:

#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |
####### V

Fixed walls are marked with #, and doors are marked with - or |.

The doors in your current room are either open or closed (and locked) based on
the hexadecimal MD5 hash of a passcode (your puzzle input) followed by a
sequence of uppercase characters representing the path you have taken so far
(U for up, D for down, L for left, and R for right).

Only the first four characters of the hash are used; they represent,
respectively, the doors up, down, left, and right from your current position.
Any b, c, d, e, or f means that the corresponding door is open; any other
character (any number or a) means that the corresponding door is closed and
locked.

To access the vault, all you need to do is reach the bottom-right room;
reaching this room opens the vault and all doors in the maze.

For example, suppose the passcode is hijkl. Initially, you have taken no steps,
and so your path is empty: you simply find the MD5 hash of hijkl alone. The
first four characters of this hash are ced9, which indicate that up is open
(c), down is open (e), left is open (d), and right is closed and locked (9).
Because you start in the top-left corner, there are no "up" or "left" doors to
be open, so your only choice is down.

Next, having gone only one step (down, or D), you find the hash of hijklD. This
produces f2bc, which indicates that you can go back up, left (but that's a
wall), or right. Going right means hashing hijklDR to get 5745 - all doors
closed and locked. However, going up instead is worthwhile: even though it
returns you to the room you started in, your path would then be DU, opening a
different set of doors.

After going DU (and then hashing hijklDU to get 528e), only the right door is
open; after going DUR, all doors lock. (Fortunately, your actual passcode is
not hijkl).

Passcodes actually used by Easter Bunny Vault Security do allow access to the
vault if you know the right path. For example:

 - If your passcode were ihgpwlah, the shortest path would be DDRRRD.
 - With kglvqrro, the shortest path would be DDUDRLRRUDRD.
 - With ulqzkmiv, the shortest would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.

Given your vault's passcode, what is the shortest path (the actual path, not
just the length) to reach the vault?
*/

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
