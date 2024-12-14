module AOC202408 where

import Data.List(nub)
import Data.Map(alter,elems,empty,keys,toList)

import AOC

aoc = AOC {
    day="08",
    testData=unlines [
    "............",
    "........0...",
    ".....0......",
    ".......0....",
    "....0.......",
    "......A.....",
    "............",
    "............",
    "........A...",
    ".........A..",
    "............",
    "............"
    ],
    testResult="14",
    testData2="",
    testResult2="34",
    aocParse=parse2d,
    aocTest=result,
    aocResult=result,
    aocParse2=parse2d,
    aocTest2=result2,
    aocResult2=result2
    }

result mp =
    length $ nub $ filter inBounds $ concatMap toAntinodes $ elems nodesByFreq
  where
    nodesByFreq = foldl collect empty $ toList mp
    (xmax,ymax) = maximum $ keys mp
    inBounds (x,y) = x >= 0 && x <= xmax && y >= 0 && y <= ymax

collect nodesByFreq (pos,ch)
  | ch == '.' = nodesByFreq
  | otherwise = alter (Just . maybe [pos] (pos:)) ch nodesByFreq

toAntinodes nodes = [antinode n1 n2 | n1 <- nodes, n2 <- nodes, n1 /= n2]
  where antinode (x1,y1) (x2,y2) = (2*x2-x1,2*y2-y1)

result2 mp = length $ nub $ concatMap (toAntinodes2 (maximum $ keys mp))
                    $ elems $ foldl collect empty $ toList mp

toAntinodes2 (xmax,ymax) nodes =
    concat [antinodes2 n1 n2 | n1 <- nodes, n2 <- nodes, n1 /= n2]
  where
    antinodes2 (x1,y1) (x2,y2) =
        takeWhile inBounds [(x1-dx*i,y1-dy*i) | i <- [0..]]
      where
        (ldx,ldy) = (x2-x1,y2-y1)
        (dx,dy) = (ldx `div` gcd ldx ldy,ldy `div` gcd ldx ldy)
    inBounds (x,y) = x >= 0 && x <= xmax && y >= 0 && y <= ymax
