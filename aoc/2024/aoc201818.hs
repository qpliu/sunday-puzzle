module AOC201818 where

import Data.Map(Map,elems,empty,findWithDefault,fromList,
                insert,member,toList,(!))

import AOC

aoc = AOC {
    day="../../2018/input/18",
    aocTests=[
        AOCTest {
            testData=unlines [
                ".#.#...|#.",
                ".....#|##|",
                ".|..|...#.",
                "..|#.....#",
                "#.#|||#|#|",
                "...#.||...",
                ".|....|...",
                "||...#|.#|",
                "|.||||..|.",
                "...#.|..|."
                ],
            testResult=Just "1147",
            testResult2=Nothing
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse2d,
        pcodeParse2=const parse2d,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

countResource :: Char -> (Int,Int) -> (Int,Int)
countResource loc counts@(ntrees,nlumber)
  | loc == '|' = (ntrees+1,nlumber)
  | loc == '#' = (ntrees,nlumber+1)
  | otherwise = (ntrees,nlumber)

evolve :: Map (Int,Int) Char -> ((Int,Int),Char) -> ((Int,Int),Char)
evolve area (xy@(x,y),loc)
  | loc == '.' && ntrees >= 3 = (xy,'|')
  | loc == '|' && nlumber >= 3 = (xy,'#')
  | loc == '#' && (ntrees == 0 || nlumber == 0) = (xy,'.')
  | otherwise = (xy,loc)
  where
    (ntrees,nlumber) =
        foldr countResource (0,0)
              (map (flip (findWithDefault ' ') area)
                   [(x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x+1,y),
                    (x-1,y+1),(x,y+1),(x+1,y+1)])

tick :: Int -> Map (Int,Int) Char -> Map (Int,Int) Char
tick ncpu area = fromList $ parallelMap ncpu (evolve area) $ toList area

resourceValue :: Map (Int,Int) Char -> Int
resourceValue = uncurry (*) . foldr countResource (0,0) . elems

result ncpu = resourceValue . head . drop 10 . iterate (tick ncpu)

findCycle :: Ord a => (a -> a) -> a -> (Int,Int,Map Int a)
findCycle f a = search 0 a empty empty
  where
    search t a seen indexed
      | member a seen = (seen!a,t,indexed)
      | otherwise = search (t+1) (f a) (insert a t seen) (insert t a indexed)

result2 ncpu input =
    resourceValue $ indexed!((1000000000-t0) `mod` (t1-t0) + t0)
  where (t0,t1,indexed) = findCycle (tick ncpu) input