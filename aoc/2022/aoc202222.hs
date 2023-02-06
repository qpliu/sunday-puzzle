-- Anticipate up to 250 columns.

import Data.Char(isDigit)
import Data.List(groupBy)
import Data.Map(Map,fromList,keys,mapWithKey,member,(!))

parse :: String -> (Map (Int,Int) Char,[String])
parse = p 1 1 []
  where
    p _ _ tiles ('\n':'\n':rest) = (fromList tiles,concatMap (groupBy isDigits) (words rest))
    p x y tiles (c:rest)
      | c == '\n' = p 1 (y+1) tiles rest
      | c == '.' || c == '#' = p (x+1) y (((x,y),c):tiles) rest
      | otherwise = p (x+1) y tiles rest
    isDigits a b = isDigit a == isDigit b

faceR :: Int
faceR = 0

faceD :: Int
faceD = 1

faceL :: Int
faceL = 2

faceU :: Int
faceU = 3

turnL :: Int -> Int
turnL dir = (dir-1) `mod` 4

turnR :: Int -> Int
turnR dir = (dir+1) `mod` 4

forward :: Int -> (Int,Int) -> (Int,Int)
forward 0 (x,y) = (x+1,y)
forward 1 (x,y) = (x,y+1)
forward 2 (x,y) = (x-1,y)
forward 3 (x,y) = (x,y-1)

backward :: Int -> (Int,Int) -> (Int,Int)
backward 0 (x,y) = (x-1,y)
backward 1 (x,y) = (x,y-1)
backward 2 (x,y) = (x+1,y)
backward 3 (x,y) = (x,y+1)

addNeighbors :: (Map (Int,Int) Char,[String]) -> (Map (Int,Int) (Char,Map Int ((Int,Int),Int)),[String])
addNeighbors (board,path) = (mapWithKey addNs board,path)
  where
    addNs xy ch = (ch,fromList [(dir,(getN xy dir,dir)) | dir <- [0..3]])
    getN xy dir
      | forward dir xy `member` board = forward dir xy
      | otherwise = wrap xy dir
    wrap xy dir
      | backward dir xy `member` board = wrap (backward dir xy) dir
      | otherwise = xy

follow :: (Map (Int,Int) (Char,Map Int ((Int,Int),Int)),[String]) -> (Int,(Int,Int))
follow (board,path) = f path startPos
  where
  f [] pos = pos
  f (insn:insns) (dir,xy)
    | insn == "L" = f insns (turnL dir,xy)
    | insn == "R" = f insns (turnR dir,xy)
    | otherwise = go (read insn) insns dir xy
  go n insns dir xy
    | n <= 0 = f insns (dir,xy)
    | member newXY board && fst (board!newXY) == '.' = go (n-1) insns newDir newXY
    | otherwise = f insns (dir,xy)
    where (newXY,newDir) = (snd (board!xy))!dir
  startPos = (faceR,minimum $ filter ((== 1) . snd) $ keys board)

password :: (Int,(Int,Int)) -> Int
password (dir,(x,y)) = y*1000+x*4+dir

testData :: String
testData = unlines [
    "        ...#",
    "        .#..",
    "        #...",
    "        ....",
    "...#.......#",
    "........#...",
    "..#....#....",
    "..........#.",
    "        ...#....",
    "        .....#..",
    "        .#......",
    "        ......#.",
    "",
    "10R5L5R10L4R5L5"
    ]

test :: ()
test
  | (password . follow . addNeighbors . parse) testData /= 6032 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (password . follow . addNeighbors . parse) $ readFile "input/22.txt"

{-
The shape of the test data is:

          nnn
          nnn
          nnn

  ttt www bbb
  ttt www bbb
  ttt www bbb

          sss eee
          sss eee
          sss eee
-}

addNeighbors2test :: (Map (Int,Int) Char,[String]) -> (Map (Int,Int) (Char,Map Int ((Int,Int),Int)),[String])
addNeighbors2test (board,path) = (mapWithKey addNs board,path)
  where
    y1 = minimum $ map snd $ filter ((== 1) . fst) $ keys board
    y2 = maximum $ map snd $ filter ((== 1) . fst) $ keys board
    x2 = minimum $ map fst $ filter ((== 1) . snd) $ keys board
    x1 = x2 - y1
    x3 = maximum $ map fst $ filter ((== 1) . snd) $ keys board
    (x4,y3) = maximum $ keys board
    addNs xy ch = (ch,fromList [(dir,getDest xy dir) | dir <- [0..3]])
    getDest xy@(x,y) dir
      | member (forward dir xy) board = ((forward dir xy),dir)
      | dir == faceU && y == 1 = ((x1+x2-x,y1),faceD)
      | dir == faceL && x == x2 && y < y1 = ((x1+y,y1),faceD)
      | dir == faceR && x == x3 && y < y1 = ((x4,y1+y2-y),faceL)
      | dir == faceU && y == y1 && x <= x1 = ((x3-x+1,1),faceD)
      | dir == faceU && y == y1 && x < x2 = ((x2,x-x1),faceR)
      | dir == faceL && x == 1 = ((x3+1+y2-y,y3),faceU)
      | dir == faceR && x == x3 && y <= y2 = ((x3+1+y2-y,y2+1),faceD)
      | dir == faceD && x <= x1 && y == y2 = ((x3-x+1,y3),faceU)
      | dir == faceD && x < x2 && y == y2 = ((x2,y2+x2-x),faceR)
      | dir == faceU && x > x3 && y == y2+1 = ((x3,y2+1+x3-x),faceL)
      | dir == faceL && x == x2 && y > y2 = ((x2+y2-y,y2),faceU)
      | dir == faceR && x == x4 = ((x3,y3-y+1),faceL)
      | dir == faceD && x <= x3 && y == y3 = ((x3-x+1,y2),faceU)
      | dir == faceD && y == y3 = ((1,y1+x4-x),faceR)
      | otherwise = error (show (xy,dir))

testNeighbors :: (Map (Int,Int) (Char,Map Int ((Int,Int),Int)),[String]) -> Bool
testNeighbors (board,_) = and [testDir xy dir | xy <- keys board, dir <- [0..3]]
  where
    testDir xy dir
      | xy == returnXY && dir == turnL (turnL returnDir) = True
      | otherwise = error (show (xy,dir,destXY,destDir,returnXY,returnDir))
      where
        (destXY,destDir) = (snd (board!xy))!dir
        (returnXY,returnDir)
          | not (member destXY board) = error (show (xy,dir,destXY,destDir))
          | otherwise = (snd (board!destXY))!(turnL (turnL destDir))

test2 :: ()
test2
  | (not . testNeighbors . addNeighbors . parse) testData = error "a"
  | (not . testNeighbors . addNeighbors2test . parse) testData = error "b"
  | (password . follow . addNeighbors2test . parse) testData /= 5031 = error "c"
  | (not . testNeighbors . addNeighbors2 . parse) testData2 = error "d"
  | otherwise = ()


{-
The shape of my input data is:

          nnn eee
          nnn eee
          nnn eee

          bbb
          bbb
          bbb

     www  sss
     www  sss
     www  sss

     ttt
     ttt
     ttt
-}

testData2 :: String
testData2 = unlines [
    "    ...#....",
    "    .#......",
    "    #.......",
    "    ........",
    "    ...#",
    "    #...",
    "    ....",
    "    ..#.",
    ".......#",
    "........",
    ".....#..",
    "........",
    "....",
    "....",
    "....",
    "....",
    "",
    "10R5L5R10L4R5L5"
    ]

addNeighbors2 :: (Map (Int,Int) Char,[String]) -> (Map (Int,Int) (Char,Map Int ((Int,Int),Int)),[String])
addNeighbors2 (board,path) = (mapWithKey addNs board,path)
  where
    x1 = minimum $ map fst $ filter ((== 1) . snd) $ keys board
    x3 = maximum $ map fst $ filter ((== 1) . snd) $ keys board
    y1 = maximum $ map snd $ filter ((== x3) . fst) $ keys board
    y2 = minimum $ map snd $ filter ((== 1) . fst) $ keys board
    y3 = maximum $ map snd $ filter ((== x1) . fst) $ keys board
    y4 = maximum $ map snd $ filter ((== 1) . fst) $ keys board
    x2 = maximum $ map fst $ filter ((== y2) . snd) $ keys board

    addNs xy ch = (ch,fromList [(dir,getDest xy dir) | dir <- [0..3]])
    getDest xy@(x,y) dir
      | member (forward dir xy) board = ((forward dir xy),dir)
      | dir == faceU && y == 1 && x <= x2 = ((1,y3+x-x1+1),faceR)
      | dir == faceU && y == 1 = ((x-x2,y4),faceU)
      | dir == faceL && x == x1 && y <= y1 = ((1,y2+y1-y),faceR)
      | dir == faceR && x == x3 = ((x2,y2+y1-y),faceL)
      | dir == faceD && x > x2 && y == y1 = ((x2,y1+x-x2),faceL)
      | dir == faceL && x == x1 && y < y2 = ((x1+y-y2,y2),faceD)
      | dir == faceR && x == x2 && y < y2 = ((x2+y-y1,y1),faceU)
      | dir == faceU && x < x1 && y == y2 = ((x1,y1+x),faceR)
      | dir == faceL && x == 1 && y <= y3 = ((x1,y1-y+y2),faceR)
      | dir == faceR && x == x2 = ((x3,y3-y+1),faceL)
      | dir == faceD && x >= x1 && y == y3 = ((x1-1,y3+x-x1+1),faceL)
      | dir == faceL && x == 1 = ((x1+y-y3-1,1),faceD)
      | dir == faceR && x == x1-1 = ((x1+y-y3-1,y3),faceU)
      | dir == faceD && y == y4 = ((x2+x,1),faceD)
      | otherwise = error (show (xy,dir))

part2 :: IO Int
part2 = fmap (password . follow . addNeighbors2 . parse) $ readFile "input/22.txt"
