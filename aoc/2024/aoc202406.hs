import Data.List(nub)
import Data.Map(empty,fromList,insert,member,toList,(!))

import AOC

aoc = AOC {
    day="06",
    testData=unlines [
    "....#.....",
    ".........#",
    "..........",
    "..#.......",
    ".......#..",
    "..........",
    ".#..^.....",
    "........#.",
    "#.........",
    "......#..."
    ],
    testResult="41",
    testData2="",
    testResult2="6",
    aocParse=parse2d,
    aocResult=result,
    aocParse2=parse2d,
    aocResult2=result2
    }

start = fst . head . filter ((== '^') . snd) . toList

turn (dx,dy) = (-dy,dx)

result mp = length $ nub $ walk (0,-1) $ start mp
  where
    walk dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = [pos]
      | mp!nextPos == '#' = walk (turn dir) pos
      | otherwise = pos : walk dir nextPos
      where nextPos = (x+dx,y+dy)

-- brute force is pretty slow, about 90s for my input
hasLoop mp = walk empty (0,-1) $ start mp
  where
    walk previous dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = False
      | member (dir,pos) previous = True
      | mp!nextPos == '#' = walk (insert (dir,pos) () previous) (turn dir) pos
      | otherwise = walk (insert (dir,pos) () previous) dir nextPos
      where nextPos = (x+dx,y+dy)

result2 mp = length $ filter makesLoop $ filter (/= (start mp)) path
  where
    makesLoop pos = hasLoop (insert pos '#' mp)
    path = nub $ walk (0,-1) $ start mp
    walk dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = [pos]
      | mp!nextPos == '#' = walk (turn dir) pos
      | otherwise = pos : walk dir nextPos
      where nextPos = (x+dx,y+dy)
