module AOC202212 where

import Data.Array(Array,array,assocs,bounds,inRange,(!))

import AOC

aoc = AOC {
    day="../../2022/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Sabqponm",
                "abcryxxl",
                "accszExk",
                "acctuvwj",
                "abdefghi"
                ],
            testResult=Just "31",
            testResult2=Just "29"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse input = ([startXY],endXY,array (bounds grid) (map fixSE squares))
  where
    grid = parse2da input
    squares = assocs grid
    startXY = fst $ head $ filter ((== 'S') . snd) squares
    endXY = fst $ head $ filter ((== 'E') . snd) squares
    fixSE (xy,ch)
      | ch == 'S' = (xy,'a')
      | ch == 'E' = (xy,'z')
      | otherwise = (xy,ch)

search :: ([(Int,Int)],(Int,Int),Array (Int,Int) Char) -> Maybe Int
search (startXY,endXY@(endX,endY),grid) = fmap heuristic path
  where
    path = astar heuristic neighbors toState done (map ((,) 0) startXY)

    inBounds = inRange (bounds grid)

    heuristic (steps,xy@(x,y)) = steps + abs (x-endX) + abs (y-endY)
    neighbors (steps,xy@(x,y)) =
        [(steps+1,nxy) | nxy <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                         inBounds nxy,
                         grid!xy == 'S' || grid!nxy == 'E'
                                        || grid!nxy <= succ (grid!xy)]
    toState = snd
    done (_,xy) = xy == endXY

result = maybe undefined id . search

result2 (_,endXY,grid) = maybe undefined id $ search (startXY,endXY,grid)
  where startXY = [xy | (xy,ch) <- assocs grid, ch == 'a']
