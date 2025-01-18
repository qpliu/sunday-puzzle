{-# LANGUAGE BangPatterns #-}
module AOC202023 where

import Data.Map.Strict(Map,findWithDefault,fromList,insert,size,(!))

import AOC

aoc = AOC {
    day="../../2020/input/23",
    aocTests=[
        AOCTest {
            testData="389125467",
            testResult=Just "67384529",
            testResult2=Just ""
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

parse = p . concatMap parseInts . map (:[])
  where p cups = (0,(fromList $ zip [0..] cups,fromList $ zip cups [0..]))

move :: Int -> (Int,(Map Int Int,Map Int Int))
      -> (Int,(Map Int Int,Map Int Int))
move maxCup !(currentPos,cupPos@(cups,pos)) =
    ((currentPos+1) `mod` maxCup,nextCupPos)
  where
    getCup p = findWithDefault (i+1) i cups where i = p `mod` maxCup
    getPos c = findWithDefault ((c-1) `mod` maxCup) c pos
    current = getCup currentPos
    c1 = getCup (currentPos+1)
    c2 = getCup (currentPos+2)
    c3 = getCup (currentPos+3)
    dest = findDest ((current-2) `mod` maxCup + 1)
    findDest dest
      | dest /= c1 && dest /= c2 && dest /= c3 = dest
      | otherwise = findDest ((dest-2) `mod` maxCup + 1)
    destPos = getPos dest

    moveCup (cup,p) (cups,pos) = (insert i cup cups,insert cup i pos)
      where i = p `mod` maxCup

    nextCupPos = foldr moveCup cupPos $
        (c1,destPos-2):(c2,destPos-1):(c3,destPos):
        [(getCup (currentPos+i),currentPos+i-3)
         | i <- [4 .. (destPos-currentPos) `mod` maxCup]]

labels :: (Map Int Int,Map Int Int) -> Int
labels (cups,pos) = sum [10^(8-i)*cups!(((pos!1)+i) `mod` 9) | i <- [1..8]]

result = labels . snd . head . drop 100 . iterate (move 9)

labels2 :: Int -> (Map Int Int,Map Int Int) -> Int
labels2 maxCup (cups,pos) = getCup (p1+1)*getCup (p1+2)
  where
    getCup p = findWithDefault (i+1) i cups where i = p `mod` maxCup
    getPos c = findWithDefault ((c-1) `mod` maxCup) c pos
    p1 = getPos 1
    
result2 = labels2 ncups . snd . head . drop nmoves . iterate (move ncups)
  where
    ncups  =  1000000
    nmoves = 10000000
