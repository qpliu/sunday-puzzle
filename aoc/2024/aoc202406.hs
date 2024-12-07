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

{-
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
-}

{-
-- this is faster, about 32s for my input
hasLoop mp dir pos = walk empty dir pos
  where
    walk previous dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = 0
      | member (dir,pos) previous = 1
      | mp!nextPos == '#' = walk (insert (dir,pos) () previous) (turn dir) pos
      | otherwise = walk (insert (dir,pos) () previous) dir nextPos
      where nextPos = (x+dx,y+dy)

result2 mp = sum $ walk empty (0,-1) $ start mp
  where
    walk tried dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = tried
      | mp!nextPos == '#' = walk tried (turn dir) pos
      | member nextPos tried = walk tried dir nextPos
      | otherwise =
          walk (insert nextPos (hasLoop (insert nextPos '#' mp) (turn dir) pos)
                       tried) dir nextPos
      where nextPos = (x+dx,y+dy)
-}

-- this is much faster, under 2s for my input
makeGraph mp = graph
  where
    graph = fromList $ concatMap connect $ toList mp
    connect (_,'#') = []
    connect (pos,_) = [((dir,pos),c dir pos) | dir <- [(0,-1),(1,0),(0,1),(-1,0)]]
    c dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = nextPos
      | not (member (dir,nextPos) graph) = pos
      | otherwise = graph!(dir,nextPos)
      where nextPos = (x+dx,y+dy)

hasLoop graph dir@(dx,dy) pos@(x,y) = walk empty (turn dir) pos
  where
    (obsx,obsy) = (x+dx,y+dy)
    walk previous dir@(dx,dy) pos@(x,y)
      | not (member (dir,pos) graph) = 0
      | member (dir,pos) previous = 1
      | x == obsx && dy > 0 && y < obsy && obsy <= nexty =
          walk (insert (dir,pos) () previous) (turn dir) (x,obsy-1)
      | x == obsx && dy < 0 && y > obsy && obsy >= nexty =
          walk (insert (dir,pos) () previous) (turn dir) (x,obsy+1)
      | y == obsy && dx > 0 && x < obsx && obsx <= nextx =
          walk (insert (dir,pos) () previous) (turn dir) (obsx-1,y)
      | y == obsy && dx < 0 && x > obsx && obsx >= nextx =
          walk (insert (dir,pos) () previous) (turn dir) (obsx+1,y)
      | otherwise =
          walk (insert (dir,pos) () previous) (turn dir) nextPos
      where nextPos@(nextx,nexty) = graph!(dir,pos)

result2 mp = sum $ walk (insert (start mp) 0 empty) (0,-1) $ start mp
  where
    graph = makeGraph mp
    walk tried dir@(dx,dy) pos@(x,y)
      | not (member nextPos mp) = tried
      | mp!nextPos == '#' = walk tried (turn dir) pos
      | member nextPos tried = walk tried dir nextPos
      | otherwise =
          walk (insert nextPos (hasLoop graph dir pos) tried) dir nextPos
      where nextPos = (x+dx,y+dy)
