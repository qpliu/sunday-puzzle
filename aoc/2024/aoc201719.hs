module AOC201719 where

import Data.Array(Array,assocs,bounds,inRange,(!))

import AOC

aoc = AOC {
    day="../../2017/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "     |          ",
                "     |  +--+    ",
                "     A  |  C    ",
                " F---|----E|--+ ",
                "     |  |  |  D ",
                "     +B-+  +--+ "
                ],
            testResult=Just $ show "ABCDEF",
            testResult2=Just "38"
            }
        ],
    aocCode=Code {
        codeParse=parse2da,
        codeParse2=parse2da,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

route :: Array (Int,Int) Char -> [(Char,Int)]
route diagram = filter (not . (`elem` "-|") . fst) $ follow start (0,1) 1
  where
    [start] = filter isStart $ assocs diagram
    isStart ((x,y),ch) = y == 0 && ch == '|'

    inBounds = inRange (bounds diagram)
    follow (xy@(x,y),ch) dxy@(dx,dy) steps
      | ch == '+' =
          head [follow (nxy,diagram!nxy) ndxy (steps+1)
                | ndxy@(ndx,ndy) <- [(dy,dx),(-dy,-dx)],
                  nxy <- [(x+ndx,y+ndy)],
                  inBounds nxy, diagram!nxy /= ' ']
      | not (inBounds nextXY) || nextCH == ' ' = [(ch,steps)]
      | otherwise = (ch,steps) : follow (nextXY,nextCH) dxy (steps+1)
      where
        nextXY = (x+dx,y+dy)
        nextCH = diagram!nextXY

result = map fst . route

result2 = snd . last . route
