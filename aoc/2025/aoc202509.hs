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

parse = addExtremes . toPairs . parseInts
  where
    toPairs (x:y:rest) = (x,y):toPairs rest
    toPairs _ = []

addExtremes pairs = (minimum $ map fst pairs,maximum $ map fst pairs,
                     minimum $ map snd pairs,maximum $ map snd pairs,
                     pairs)

area (x1,y1) (x2,y2) = (1 + abs (x1-x2))*(1 + abs (y1-y2))

result (xmin,xmax,ymin,ymax,pairs) =
    maximum [area p1 p2 | (_,p1) <- candidates, (_,p2) <- candidates, p1 < p2]
  where
    withDist2 x0 y0 (x,y) = ((x-x0)^2+(y-y0)^2,(x,y))
    candidates = take 10 (sort (map (withDist2 xmin ymin) pairs))
              ++ take 10 (sort (map (withDist2 xmax ymin) pairs))
              ++ take 10 (sort (map (withDist2 xmin ymax) pairs))
              ++ take 10 (sort (map (withDist2 xmax ymax) pairs))

possibleCornerPair (upperLeft,ulConvex) (lowerRight,lrConvex) True =
    (upperLeft == 'F') == ulConvex && (lowerRight == 'J') == lrConvex
possibleCornerPair (lowerLeft,llConvex) (upperRight,urConvex) False =
    (lowerLeft == 'L') == llConvex && (upperRight == '7') == urConvex

result2 (xmin,xmax,ymin,ymax,input) =
    maximum $ [a
               | (xc1,yc1) <- cpoints, (xc2,yc2) <- cpoints,
                 xc1 < xc2, yc1 /= yc2,
                 possibleCornerPair (cgrid!(xc1,yc1)) (cgrid!(xc2,yc2))
                                    (yc2 > yc1),
                 a <- [area (xbig!xc1,ybig!yc1) (xbig!xc2,ybig!yc2)],
                 a > heuristicMinArea,
                 and [member (xc,yc) cgrid
                      | xc <- [xc1..xc2], yc <- [yc1,yc2]],
                 and [member (xc,yc) cgrid
                      | xc <- [xc1,xc2], yc <- [min yc1 yc2 .. max yc1 yc2]],
                 and [findWithDefault ('.',True) (xc,yc) cgrid == ('#',False)
                      | xc <- [xc1+1..xc2-1],
                        yc <- [min yc1 yc2 + 1 .. max yc1 yc2 - 1]]]
  where
    heuristicMinArea = area (xmin,ymin) (xmax,ymax) `div` 6
    xcompact = fromList $ zip (sort $ nub $ map fst input) [0..]
    ycompact = fromList $ zip (sort $ nub $ map snd input) [0..]
    xbig = fromList $ map swap $ toList xcompact
    ybig = fromList $ map swap $ toList ycompact
    xcmax = maximum xcompact
    ycmax = maximum ycompact
    cpoints = map (\ (x,y) -> (xcompact!x,ycompact!y)) input
    loop = fromList $ makeLoop (last cpoints:cpoints ++ take 1 cpoints)
    cgrid = fromList $ fillLoop 0 0 False
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
          if inLoop then ((x,y),('#',False)):fillLoop (x+1) y inLoop
                    else fillLoop (x+1) y inLoop
      | loop!(x,y) == '|' = ((x,y),('|',False)):fillLoop (x+1) y (not inLoop)
      | loop!(x,y) == 'F' =
          ((x,y),('F',not inLoop)):fillLoop (x+1) y (not inLoop)
      | loop!(x,y) == 'L' =
          ((x,y),('L',not inLoop)):fillLoop (x+1) y inLoop
      | loop!(x,y) == 'J' =
          ((x,y),('J',not inLoop)):fillLoop (x+1) y inLoop
      | loop!(x,y) == '7' =
          ((x,y),('7',inLoop)):fillLoop (x+1) y (not inLoop)
      | loop!(x,y) == '-' = ((x,y),('-',False)):fillLoop (x+1) y inLoop
