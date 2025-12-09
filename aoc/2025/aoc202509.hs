module AOC202509 where

import Data.List(nub,sort)
import Data.Map(fromList,insert,findWithDefault,member,toList,union,(!))
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "7,1",
                "11,1",
                "11,7",
                "9,7",
                "9,5",
                "2,5",
                "2,3",
                "7,3"
            ],
            testResult=Just "50",
            testResult2=Just "24"
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

parse = toPairs . parseInts
  where
    toPairs (x:y:rest) = (x,y):toPairs rest
    toPairs _ = []

result input = maximum [(1 + abs (x1-x2))*(1 + abs (y1-y2)) | p1@(x1,y1) <- input, p2@(x2,y2) <- input, p1 < p2]

result2 input =
    maximum $ [(1 + abs (xbig!xc2-xbig!xc1))*(1 + abs (ybig!yc2-ybig!yc1))
               | (xc1,yc1) <- cpoints, (xc2,yc2) <- cpoints,
                 xc1 < xc2, yc1 /= yc2,
                 and [member (xc,yc) cgrid
                      | xc <- [xc1..xc2], yc <- [yc1,yc2]],
                 and [member (xc,yc) cgrid
                      | xc <- [xc1,xc2], yc <- [min yc1 yc2 .. max yc1 yc2]],
                 and [findWithDefault '.' (xc,yc) cgrid == '#'
                      | xc <- [xc1+1..xc2-1], yc <- [min yc1 yc2 + 1 .. max yc1 yc2 - 1]]]
  where
    xcompact = fromList $ zip (sort $ nub $ map fst input) [0..]
    ycompact = fromList $ zip (sort $ nub $ map snd input) [0..]
    xbig = fromList $ map swap $ toList xcompact
    ybig = fromList $ map swap $ toList ycompact
    xcmax = maximum xcompact
    ycmax = maximum ycompact
    cpoints = map (\ (x,y) -> (xcompact!x,ycompact!y)) input
    loop = fromList $ makeLoop (last cpoints:cpoints ++ take 1 cpoints)
    cgrid = union loop $ fromList $ fillLoop 0 0 False
    makeLoop ((x1,y1):rest@((x2,y2):(x3,y3):_))
      | x1 < x2 && y2 < y3 = ((x2,y2),'7') : [((x2,y),'|') | y <- [y2+1 .. y3-1]] ++ makeLoop rest
      | x1 < x2 && y2 > y3 = ((x2,y2),'J') : [((x2,y),'|') | y <- [y3+1 .. y2-1]] ++ makeLoop rest
      | x1 > x2 && y2 < y3 = ((x2,y2),'F') : [((x2,y),'|') | y <- [y2+1 .. y3-1]] ++ makeLoop rest
      | x1 > x2 && y2 > y3 = ((x2,y2),'L') : [((x2,y),'|') | y <- [y3+1 .. y2-1]] ++ makeLoop rest
      | y1 < y2 && x2 < x3 = ((x2,y2),'L') : [((x,y2),'-') | x <- [x2+1 .. x3-1]] ++ makeLoop rest
      | y1 < y2 && x2 > x3 = ((x2,y2),'J') : [((x,y2),'-') | x <- [x3+1 .. x2-1]] ++ makeLoop rest
      | y1 > y2 && x2 < x3 = ((x2,y2),'F') : [((x,y2),'-') | x <- [x2+1 .. x3-1]] ++ makeLoop rest
      | y1 > y2 && x2 > x3 = ((x2,y2),'7') : [((x,y2),'-') | x <- [x3+1 .. x2-1]] ++ makeLoop rest
    makeLoop _ = []
    fillLoop x y inLoop
      | y > ycmax = if inLoop then error "y > ycmax" else []
      | x > xcmax = if inLoop then error ("x > xcmax, y=" ++ show y) else fillLoop 0 (y+1) False
      | not (member (x,y) loop) =
          if inLoop then ((x,y),'#'):fillLoop (x+1) y inLoop
                    else fillLoop (x+1) y inLoop
      | loop!(x,y) == '|' = fillLoop (x+1) y (not inLoop)
      | loop!(x,y) == 'F' = fillLoop (x+1) y (not inLoop)
      | loop!(x,y) == 'L' = fillLoop (x+1) y inLoop
      | loop!(x,y) == 'J' = fillLoop (x+1) y inLoop
      | loop!(x,y) == '7' = fillLoop (x+1) y (not inLoop)
      | otherwise = fillLoop (x+1) y inLoop
