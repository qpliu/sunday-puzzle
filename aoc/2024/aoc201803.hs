module AOC201803 where

import Data.Set(Set,empty,fromList,intersection,size,union)

import AOC

aoc = AOC {
    day="../../2018/input/03",
    aocTests=[
        AOCTest {
            testData = unlines [
                "#1 @ 1,3: 4x4",
                "#2 @ 3,1: 4x4",
                "#3 @ 5,5: 2x2"
                ],
            testResult=Just "4",
            testResult2=Just "3"
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

type XY = (Int,Int)

parse = map (toClaim . parseInts) . lines
  where
    toClaim [claimID,xc,yc,w,h] =
        (claimID,fromList [(x,y) | x <- [xc..xc+w-1], y <- [yc..yc+h-1]])

overlap :: [(Int,Set XY)] -> Set XY
overlap = fst . foldr collect (empty,empty)
  where
    collect (_,claim) (overlapping,all) =
        (union overlapping (intersection claim all),union claim all)

result = size . overlap

result2 claims = head [claimID | (claimID,claim) <- claims,
                                 null (intersection claim overlapping)]
  where overlapping = overlap claims
