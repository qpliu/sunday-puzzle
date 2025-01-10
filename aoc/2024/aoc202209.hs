module AOC202209 where

import Data.Set(Set,fromList,size)

import AOC

aoc = AOC {
    day="../../2022/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "R 4",
                "U 4",
                "L 3",
                "D 1",
                "R 4",
                "D 1",
                "L 5",
                "R 2"
                ],
            testResult=Just "13",
            testResult2=Just "1"
            },
        AOCTest {
            testData=unlines [
                "R 5",
                "U 8",
                "L 8",
                "D 3",
                "R 17",
                "D 10",
                "L 25",
                "U 20"
                ],
            testResult=Nothing,
            testResult2=Just "36"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 1,
        codeTest2=result 9,
        codeResult=result 1,
        codeResult2=result 9
        }
    }

parse :: String -> [(String,Int)]
parse = map (p . words) . lines
  where p [dir,count] = (dir,read count)

move :: Int -> (Int,Int) -> (Int,Int) -> [(Int,Int)] -> [(String,Int)]
     -> [(Int,Int)]
move _ _ _ _ [] = []
move knotCount (headX,headY) tailXY@(tailX,tailY) knots ((dir,mCount):rest)
  | inline = tailMoves ++ move knotCount movedHead movedTail movedKnots rest
  | otherwise = tailMoves ++ move knotCount movedHead movedTail movedKnots
                                  ((dir,mCount-1):rest)
  where
    inline = (headX == tailX && abs (headY-tailY) == knotCount
                             && (dir == "U" || dir == "D"))
          || (headY == tailY && abs (headX-tailX) == knotCount
                             && (dir == "L" || dir == "R"))
          || mCount == 1
    count | inline = mCount | otherwise = 1

    movedHead
      | dir == "U" = (headX,headY-count)
      | dir == "R" = (headX+count,headY)
      | dir == "D" = (headX,headY+count)
      | dir == "L" = (headX-count,headY)
    (movedKnots,tailMoves,movedTail) = moveKnots movedHead knots []

    moveKnots (leadX,leadY) ((x,y):follows) leads
      | abs (leadX-x) <= 1 && abs (leadY-y) <= 1 =
          (reverse leads++(x,y):follows,[],tailXY)
      | not (null follows) = moveKnots targetXY follows (targetXY:leads)
      | otherwise =
          (reverse (targetXY:leads),trail (x,y) targetXY,targetXY)
      where
        targetXY
          | abs (leadX-x) == abs (leadY-y) =
              (leadX-signum (leadX-x),leadY-signum (leadY-y))
          | abs (leadX-x) > abs (leadY-y) = (leadX-signum (leadX-x),leadY)
          | abs (leadX-x) < abs (leadY-y) = (leadX,leadY-signum (leadY-y))

    trail xy@(x,y) endXY@(endX,endY)
      | xy == endXY = [xy]
      | otherwise = xy : trail (x+signum (endX-x),y+signum (endY-y)) endXY

result n = size . fromList . ((0,0):) . move n (0,0) (0,0) (replicate n (0,0))
