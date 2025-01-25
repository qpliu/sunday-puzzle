module AOC201924 where

import Data.Array(assocs)
import Data.Bits(clearBit,complementBit,popCount,setBit,(.&.))
import Data.Set(empty,insert,member)

import AOC

aoc = AOC {
    day="../../2019/input/24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "....#",
                "#..#.",
                "#..##",
                "..#..",
                "#...."
                ],
            testResult=Just "2129920",
            testResult2=Just "99"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=(:[]) . parse,
        codeTest=result,
        codeTest2=result2 10,
        codeResult=result,
        codeResult2=result2 200
        }
    }

parse = foldr collect 0 . assocs . parse2da
  where
    collect ((x,y),'#') bugs = setBit bugs (x+y*5)
    collect _ bugs = bugs

neighbors :: [(Int,Int)]
neighbors = [
    (b 0,  n [      1, 5]),
    (b 1,  n [   0, 2, 6]),
    (b 2,  n [   1, 3, 7]),
    (b 3,  n [   2, 4, 8]),
    (b 4,  n [   3,    9]),
    (b 5,  n [0,    6, 10]),
    (b 6,  n [1, 5, 7, 11]),
    (b 7,  n [2, 6, 8, 12]),
    (b 8,  n [3, 7, 9, 13]),
    (b 9,  n [4, 8,    14]),
    (b 10, n [5,    11,15]),
    (b 11, n [6, 10,12,16]),
    (b 12, n [7, 11,13,17]),
    (b 13, n [8, 12,14,18]),
    (b 14, n [9, 13,   19]),
    (b 15, n [10,   16,20]),
    (b 16, n [11,15,17,21]),
    (b 17, n [12,16,18,22]),
    (b 18, n [13,17,19,23]),
    (b 19, n [14,18,   24]),
    (b 20, n [15,   21]),
    (b 21, n [16,20,22]),
    (b 22, n [17,21,23]),
    (b 23, n [18,22,24]),
    (b 24, n [19,23])
    ]
  where
    b = setBit 0
    n = foldl setBit 0

life :: Int -> Int
life bugs = sum $ map bug neighbors
  where
    bug (b,nbors)
      | n == 1 || (n == 2 && (b .&. bugs) == 0) = b
      | otherwise = 0
      where n = popCount (nbors .&. bugs)

findRecurrence :: Ord a => (a -> a) -> a -> a
findRecurrence f a = search a empty
  where
    search a seen
      | member a seen = a
      | otherwise = search (f a) (insert a seen)

result = findRecurrence life

neighbors2 :: [(Int,(Int,Int,Int))]
neighbors2 = [
    (b 0,  (n [7,11      ], n [      1, 5 ], n [])),
    (b 1,  (n [7         ], n [   0, 2, 6 ], n [])),
    (b 2,  (n [7         ], n [   1, 3, 7 ], n [])),
    (b 3,  (n [7         ], n [   2, 4, 8 ], n [])),
    (b 4,  (n [7,   13   ], n [   3,    9 ], n [])),
    (b 5,  (n [  11      ], n [0,    6, 10], n [])),
    (b 6,  (n [          ], n [1, 5, 7, 11], n [])),
    (b 7,  (n [          ], n [2, 6, 8    ], n [0,1,2,3,4])),
    (b 8,  (n [          ], n [3, 7, 9, 13], n [])),
    (b 9,  (n [     13   ], n [4, 8,    14], n [])),
    (b 10, (n [  11      ], n [5,    11,15], n [])),
    (b 11, (n [          ], n [6, 10,   16], n [0,5,10,15,20])),
    (b 13, (n [          ], n [8,    14,18], n [4,9,14,19,24])),
    (b 14, (n [     13   ], n [9, 13,   19], n [])),
    (b 15, (n [  11      ], n [10,   16,20], n [])),
    (b 16, (n [          ], n [11,15,17,21], n [])),
    (b 17, (n [          ], n [   16,18,22], n [20,21,22,23,24])),
    (b 18, (n [          ], n [13,17,19,23], n [])),
    (b 19, (n [     13   ], n [14,18,   24], n [])),
    (b 20, (n [  11,   17], n [15,   21],    n [])),
    (b 21, (n [        17], n [16,20,22],    n [])),
    (b 22, (n [        17], n [17,21,23],    n [])),
    (b 23, (n [        17], n [18,22,24],    n [])),
    (b 24, (n [     13,17], n [19,23],       n []))
    ]
  where
    b = setBit 0
    n = foldl setBit 0

life2 :: [Int] -> [Int]
life2 bugs = trim $ zipWith3 level (0:0:bugs) (0:bugs ++ [0]) (bugs ++ [0,0])
  where
    level up here down = sum $ map bug neighbors2
      where
        bug (b,(unbors,nbors,dnbors))
          | n == 1 || (n == 2 && (b .&. here) == 0) = b
          | otherwise = 0
          where n = popCount (unbors .&. up) + popCount (nbors .&. here)
                                             + popCount (dnbors .&. down)
    trim = maybe [] id . trimRight . dropWhile (== 0)
    trimRight [] = Nothing
    trimRight (n:ns)
      | n == 0 = maybe Nothing (Just . (0:)) $ trimRight ns
      | otherwise = Just $ maybe [n] (n:) $ trimRight ns

result2 n = sum . map popCount . head . drop n . iterate life2
