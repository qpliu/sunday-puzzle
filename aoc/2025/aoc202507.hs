module AOC202507 where

import Data.Map(alter,empty,insert,member,toList,(!))
import Data.Set(elems,fromList,size)

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
        codeParse=parse2d,
        codeParse2=parse2d,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result manifold = size $ fromList splits
  where
    splits = sendBeams (map fst $ filter ((== 'S') . snd) $ toList manifold)

    sendBeams beams
      | null nextBeams = newSplits
      | otherwise = newSplits ++ sendBeams (elems $ fromList nextBeams)
      where
        (newSplits,nextBeams) = foldr sendBeam ([],[]) beams
    sendBeam (x,y) (splitAcc,beamAcc)
      | not $ member (x,y) manifold = (splitAcc,beamAcc)
      | manifold!(x,y) == 'S' = (splitAcc,(x,y+1):beamAcc)
      | manifold!(x,y) == '.' = (splitAcc,(x,y+1):beamAcc)
      | manifold!(x,y) == '^' = ((x,y):splitAcc,(x-1,y):(x+1,y):beamAcc)

result2 manifold = sum $ sendBeams $ mergeBeams $ map (fmap (const 1))
                                   $ filter ((== 'S') . snd) $ toList manifold
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
