module AOC202507 where

import Data.List(nub)
import Data.Map(alter,empty,insert,member,size,toList,(!))

import AOC

aoc = AOC {
    day="07",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".......S.......",
                "...............",
                ".......^.......",
                "...............",
                "......^.^......",
                "...............",
                ".....^.^.^.....",
                "...............",
                "....^.^...^....",
                "...............",
                "...^.^...^.^...",
                "...............",
                "..^...^.....^..",
                "...............",
                ".^.^.^.^.^...^.",
                "..............."
            ],
            testResult=Just "21",
            testResult2=Just "40"
            }
        ],
    aocCode=Code {
        codeParse=addStart . parse2d,
        codeParse2=addStart . parse2d,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

addStart manifold =
    (map fst $ filter ((== 'S') . snd) $ toList manifold,manifold)

result (start,manifold) = size $ sendBeams empty start
  where
    sendBeams splits beams
      | null nextBeams = splits
      | otherwise = sendBeams newSplits (nub nextBeams)
      where
        (newSplits,nextBeams) = foldr sendBeam (splits,[]) beams
    sendBeam (x,y) (splits,beams)
      | not $ member (x,y+1) manifold = (splits,beams)
      | manifold!(x,y+1) == '.' = (splits,(x,y+1):beams)
      | manifold!(x,y+1) == '^' = (insert (x,y) () splits,
                                   (x-1,y+1):(x+1,y+1):beams)

result2 (start,manifold) =
    sum $ sendBeams $ mergeBeams $ map (flip (,) 1) start
  where
    sendBeams beams
      | null nextBeams = beams
      | otherwise = sendBeams $ mergeBeams nextBeams
      where
        nextBeams = concatMap sendBeam $ toList beams
    sendBeam ((x,y),n)
      | not $ member (x,y+1) manifold = []
      | manifold!(x,y+1) == '.' = [((x,y+1),n)]
      | manifold!(x,y+1) == '^' = [((x-1,y+1),n),((x+1,y+1),n)]
    mergeBeams = foldr merge empty
      where
        merge (xy,n) nextBeams = alter (Just . maybe n (n+)) xy nextBeams
