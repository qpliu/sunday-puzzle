module AOC202322 where

import Data.List(sortBy)
import Data.Map(Map,adjust,empty,fromList,insert,keys,toList,(!))
import Data.Set(Set,difference,elems,member,size,union)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2023/input/22",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1,0,1~1,2,1",
                "0,0,2~2,0,2",
                "0,2,3~2,2,3",
                "0,0,4~0,2,4",
                "2,0,5~2,2,5",
                "0,1,6~2,1,6",
                "1,1,8~1,1,9"
                ],
            testResult=Just "5",
            testResult2=Just "7"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse input =
    fall (zip [1..] bricks,minimum xs,minimum ys,maximum xs,maximum ys)
  where
    bricks = sortBy z $ map parseInts $ lines input
    xs = map (head . drop 0) bricks ++ map (head . drop 3) bricks
    ys = map (head . drop 1) bricks ++ map (head . drop 4) bricks
    z [_,_,z11,_,_,z12] [_,_,z21,_,_,z22] = compare (min z11 z12) (min z21 z22)

fall :: ([(Int,[Int])],Int,Int,Int,Int)
     -> (Map Int (Set Int),Map Int (Set Int))
fall (bricks,xmin,ymin,xmax,ymax) =
    snd $ foldl fall1 (initialTop,(initialSupports,empty)) bricks
  where
    initialTop =
        fromList [((x,y),(0,0)) | x <- [xmin..xmax], y <- [ymin..ymax]]
    initialSupports = fromList $ map (fmap (const Data.Set.empty)) bricks
    fall1 (top,(supports,supportedBy)) (brick,[x1,y1,z1,x2,y2,z2]) =
        (newTop,(newSupports,newSupportedBy))
      where
        xys = [(x,y) | x <- [min x1 x2..max x1 x2],y <- [min y1 y2..max y1 y2]]
        z = maximum [fst $ top!xy | xy <- xys]
        ztop = z + 1 + abs (z1 - z2)
        newTop = foldr (flip insert (ztop,brick)) top xys

        supBy = Data.Set.fromList $ filter (>0)
                                  $ [snd $ top!xy | xy <- xys,
                                                    fst (top!xy) == z]
        newSupportedBy = insert brick supBy supportedBy

        newSupports =
            foldr (adjust (Data.Set.insert brick)) supports $ elems supBy

result ncpu (supports,supportedBy) =
    length [() | (_,support) <- toList supports,
                 all ((> 1) . size) [supportedBy!b | b <- elems support]]

countFalls :: (Map Int (Set Int),Map Int (Set Int)) -> Int -> Int
countFalls (supports,supportedBy) brick =
    size (snd $ falls brick (Data.Set.empty,Data.Set.fromList [brick])) - 1
  where
    falls brick (processed,fallen)
      | member brick processed = (processed,fallen)
      | otherwise =
            foldr falls (Data.Set.insert brick processed,
                         union (Data.Set.fromList willFall) fallen)
                        willFall
      where
        willFall = [b | b <- elems (supports!brick),
                        null (difference (supportedBy!b) fallen)]

result2 ncpu input@(supports,_) =
    parallelMapReduce ncpu (countFalls input) sum $ keys supports
