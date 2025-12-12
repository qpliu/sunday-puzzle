module AOC202509 where

import Data.List(nub,sort)
import Data.Map(empty,fromList,insert,findWithDefault,(!))

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
    maximum [area p1 p2 | (_,p1) <- candidates1, (_,p2) <- candidates2]
  where
    withDist2 x0 y0 (x,y) = ((x-x0)^2+(y-y0)^2,(x,y))
    -- heuristic: expect the largest rectangle to have corners that are
    -- one of the closest to opposite corners of the bounding rectangle
    candidates1 = take 5 (sort (map (withDist2 xmin ymin) pairs))
               ++ take 5 (sort (map (withDist2 xmin ymax) pairs))
    candidates2 = take 5 (sort (map (withDist2 xmax ymin) pairs))
               ++ take 5 (sort (map (withDist2 xmax ymax) pairs))
 
result2 (xmin,xmax,ymin,ymax,pairs) =
    -- heuristic based on shape of the input: the largest rectangle will
    -- have a corner that is one of the 4 corners connected by one of the
    -- two longest segments to one of its neighbors
    scanRectangles 1 (take 4 corners) corners
  where
    xcompact = fromList $ zip (sort $ nub $ map fst pairs) [0..]
    ycompact = fromList $ zip (sort $ nub $ map snd pairs) [0..]

    -- sort by negative of the length of longest segment to neighbor
    -- so that the longest ones come first
    corners = sort $ makeCorners (last pairs:pairs ++ [head pairs])
    makeCorners ((x1,y1):rest@((x2,y2):(x3,y3):_)) =
        (- maximum [abs (x1-x2),abs (y1-y2),abs (x2-x3),abs (y2-y3)],
         (xcompact!x2,ycompact!y2),(x2,y2)) : makeCorners rest
    makeCorners _ = []

    scanRectangles curMaxArea [] _ = curMaxArea
    scanRectangles curMaxArea (_:corners1) [] =
        scanRectangles curMaxArea corners1 corners
    scanRectangles curMaxArea corners1@(c1@(_,_,xy1):_)
                              (c2@(_,_,xy2):corners2)
      | curArea <= curMaxArea || hsegThrough c1 c2 || vsegThrough c1 c2 =
          scanRectangles curMaxArea corners1 corners2
      | otherwise =
          scanRectangles curArea corners1 corners2
      where curArea = area xy1 xy2
    
    hsegs = makeHsegs empty (last pairs:pairs)
      where
        makeHsegs segs ((x1,y1):rest@((x2,y2):_))
          | x1 == x2 = makeHsegs segs rest
          | otherwise =
              makeHsegs (insert (ycompact!y1)
                                ((xcompact!(min x1 x2),xcompact!(max x1 x2)) :
                                 findWithDefault [] (ycompact!y1) segs)
                                segs)
                        rest
        makeHsegs segs _ = segs
    hsegThrough (_,(xc1,yc1),_) (_,(xc2,yc2),_) =
        not $ null [() | yc <- [min yc1 yc2 + 1 .. max yc1 yc2 - 1],
                         not $ null [() | (xseg1,xseg2) <- hsegs!yc,
                                          min xc1 xc2 < xseg2,
                                          max xc1 xc2 > xseg1]]

    vsegs = makeVsegs empty (last pairs:pairs)
      where
        makeVsegs segs ((x1,y1):rest@((x2,y2):_))
          | y1 == y2 = makeVsegs segs rest
          | otherwise =
              makeVsegs (insert (xcompact!x1)
                                ((ycompact!(min y1 y2),ycompact!(max y1 y2)) :
                                 findWithDefault [] (xcompact!x1) segs)
                                segs)
                        rest
        makeVsegs segs _ = segs
    vsegThrough (_,(xc1,yc1),_) (_,(xc2,yc2),_) =
        not $ null [() | xc <- [min xc1 xc2 + 1 .. max xc1 xc2 - 1],
                         not $ null [() | (yseg1,yseg2) <- vsegs!xc,
                                          min yc1 yc2 < yseg2,
                                          max yc1 yc2 > yseg1]]
