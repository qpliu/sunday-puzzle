module AOC201823 where

import AOC

aoc = AOC {
    day="../../2018/input/23",
    aocTests=[
        AOCTest {
            testData=unlines [
                "pos=<0,0,0>, r=4",
                "pos=<1,0,0>, r=1",
                "pos=<4,0,0>, r=3",
                "pos=<0,2,0>, r=1",
                "pos=<0,5,0>, r=3",
                "pos=<0,0,3>, r=1",
                "pos=<1,1,1>, r=1",
                "pos=<1,1,2>, r=1",
                "pos=<1,3,1>, r=1"
                ],
            testResult=Just "7",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "pos=<10,12,12>, r=2",
                "pos=<12,14,12>, r=2",
                "pos=<16,12,12>, r=4",
                "pos=<14,14,14>, r=6",
                "pos=<50,50,50>, r=200",
                "pos=<10,10,10>, r=5"
                ],
            testResult=Nothing,
            testResult2=Just "36"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=const result,
        pcodeTest2=result2,
        pcodeResult=const result,
        pcodeResult2=result2
        }
    }

type Bot = (Int,Int,Int,Int)

parse = map (p . parseInts) . lines
  where p [x,y,z,r] = (r,x,y,z)

inRangeOf :: Bot -> Bot -> Bool
inRangeOf (_,x1,y1,z1) (r,x2,y2,z2) =
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2) <= r

result bots = length $ filter (`inRangeOf` strongest) bots
  where strongest = maximum bots

findBestPosition :: Int -> [Bot] -> ((Int,Int),(Int,Int),(Int,Int))
          -> (Int,Int,Int,Int)
findBestPosition ncpu bots ((xmin,xmax),(ymin,ymax),(zmin,zmax)) =
    parallelMapReduce ncpu count bestPosition
                      [(0,x,y,z) | x <- [xmin..xmax],
                                   y <- [ymin..ymax],
                                   z <- [zmin..zmax]]
  where count loc@(_,x,y,z) = (length $ filter (inRangeOf loc) bots,x,y,z)

bestPosition :: [(Int,Int,Int,Int)] -> (Int,Int,Int,Int)
bestPosition = snd . minimum . map addMetric
  where addMetric pos@(r,x,y,z) = ((-r,abs x+abs y+abs z),pos)

initialRange :: [Bot] -> ((Int,Int),(Int,Int),(Int,Int))
initialRange bots@((_,x0,y0,z0):_) =
    foldr minmax ((x0,x0),(y0,y0),(z0,z0)) bots
  where
    minmax (_,x,y,z) ((xmin,xmax),(ymin,ymax),(zmin,zmax)) =
        ((min x xmin,max x xmax),
         (min y ymin,max y ymax),
         (min z zmin,max z xmax))

refineRange :: Int -> [Bot] -> ((Int,Int),(Int,Int),(Int,Int))
            -> Maybe ((Int,Int),(Int,Int),(Int,Int))
refineRange ncpu bots ((xmin,xmax),(ymin,ymax),(zmin,zmax))
  | factor <= 1 = Nothing
  | otherwise = Just (((xb-1)*factor,(xb+1)*factor),
                      ((yb-1)*factor,(yb+1)*factor),
                      ((zb-1)*factor,(zb+1)*factor))
  where
    scale = maximum [xmax-xmin,ymax-ymin,zmax-zmin]
    factor = scale `div` (10*ncpu)

    scaleBot (r,x,y,z) = ((r + factor - 1) `div` factor,
                          x `div` factor, y `div` factor, z `div` factor)
    scaledRange = ((xmin `div` factor, xmax `div` factor),
                   (ymin `div` factor, ymax `div` factor),
                   (zmin `div` factor, zmax `div` factor))
    (_,xb,yb,zb) = findBestPosition ncpu (map scaleBot bots) scaledRange

find :: Int -> [Bot] -> (Int,Int,Int,Int)
find ncpu bots = findBestPosition ncpu bots $ refine $ initialRange bots
  where refine r = maybe r refine $ refineRange ncpu bots r

result2 ncpu = dist . find 1 -- ncpu > 1 doesn't work for some reason
  where dist (_,x,y,z) = abs x + abs y + abs z
