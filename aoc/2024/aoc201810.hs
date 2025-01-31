module AOC201809 where

import qualified Data.Map
import Data.Set(Set,fromList)

import AOC

aoc = AOC {
    day="../../2018/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "position=< 9,  1> velocity=< 0,  2>",
                "position=< 7,  0> velocity=<-1,  0>",
                "position=< 3, -2> velocity=<-1,  1>",
                "position=< 6, 10> velocity=<-2, -1>",
                "position=< 2, -4> velocity=< 2,  2>",
                "position=<-6, 10> velocity=< 2, -2>",
                "position=< 1,  8> velocity=< 1, -1>",
                "position=< 1,  7> velocity=< 1,  0>",
                "position=<-3, 11> velocity=< 1, -2>",
                "position=< 7,  6> velocity=<-1, -1>",
                "position=<-2,  3> velocity=< 1,  0>",
                "position=<-4,  3> velocity=< 2,  0>",
                "position=<10, -3> velocity=<-1,  1>",
                "position=< 5, 11> velocity=< 1, -2>",
                "position=< 4,  7> velocity=< 0, -1>",
                "position=< 8, -2> velocity=< 0,  1>",
                "position=<15,  0> velocity=<-2,  0>",
                "position=< 1,  6> velocity=< 1,  0>",
                "position=< 8,  9> velocity=< 0, -1>",
                "position=< 3,  3> velocity=<-1,  1>",
                "position=< 0,  5> velocity=< 0, -1>",
                "position=<-2,  2> velocity=< 2,  0>",
                "position=< 5, -2> velocity=< 1,  2>",
                "position=< 1,  4> velocity=< 2,  1>",
                "position=<-2,  7> velocity=< 2, -2>",
                "position=< 3,  6> velocity=<-1, -1>",
                "position=< 5,  0> velocity=< 1,  0>",
                "position=<-6,  0> velocity=< 2,  0>",
                "position=< 5,  9> velocity=< 1, -2>",
                "position=<14,  7> velocity=<-2,  0>",
                "position=<-3,  6> velocity=< 2, -1>"
                ],
            testResult=Just $ show "HI",
            testResult2=Just "3"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result 8,
        codeTest2=result2 8,
        codeResult=result 10,
        codeResult2=result2 10
        }
    }

parse = map parseInts . filter (not . null) . lines

yRange :: [[Int]] -> Int -> (Int,Int)
yRange points t = (maximum ys,minimum ys)
  where
    ys = map y points
    y [_,y0,_,v] = y0+v*t

findT :: Int -> [[Int]] -> Int
findT height points =
    head $ filter ((< height) . uncurry (-) . yRange points) [0..]

toImg :: [[Int]] -> Int -> Set (Int,Int)
toImg points t = fromList $ map toXY points
  where toXY [x0,y0,vx,vy] = (x0+vx*t,y0+vy*t)

ocr5x8 :: String -> String
ocr5x8 = recognize . lines
  where
    recognize pixels
      | null $ head pixels = []
      | otherwise = chars Data.Map.! (map (take 5) pixels)
                  : recognize (map (drop 6) pixels)
    chars = Data.Map.fromList
        [(["#...#",
           "#...#",
           "#...#",
           "#####",
           "#...#",
           "#...#",
           "#...#",
           "#...#"
          ],'H'),
         ([".###.",
           "..#..",
           "..#..",
           "..#..",
           "..#..",
           "..#..",
           "..#..",
           ".###."
          ],'I'),
         ([".###",
           "..#.",
           "..#.",
           "..#.",
           "..#.",
           "..#.",
           "..#.",
           ".###"
          ],'I')
        ]

ocr6x10 :: String -> String
ocr6x10 = recognize . lines
  where
    recognize pixels
      | null $ head pixels = []
      | otherwise = chars Data.Map.! (map (take 6) pixels)
                  : recognize (map (drop 8) pixels)
    chars = Data.Map.fromList
        [(["..##..",
           ".#..#.",
           "#....#",
           "#....#",
           "#....#",
           "######",
           "#....#",
           "#....#",
           "#....#",
           "#....#"
          ],'A'),
         (["######",
           "#.....",
           "#.....",
           "#.....",
           "#####.",
           "#.....",
           "#.....",
           "#.....",
           "#.....",
           "######"
          ],'E'),
         (["#....#",
           "#....#",
           "#....#",
           "#....#",
           "######",
           "#....#",
           "#....#",
           "#....#",
           "#....#",
           "#....#"
          ],'H'),
         (["...###",
           "....#.",
           "....#.",
           "....#.",
           "....#.",
           "....#.",
           "....#.",
           "#...#.",
           "#...#.",
           ".###.."
          ],'J'),
         (["#....#",
           "#...#.",
           "#..#..",
           "#.#...",
           "##....",
           "##....",
           "#.#...",
           "#..#..",
           "#...#.",
           "#....#"
          ],'K'),
         (["#....#",
           "##...#",
           "##...#",
           "#.#..#",
           "#.#..#",
           "#..#.#",
           "#..#.#",
           "#...##",
           "#...##",
           "#....#"
          ],'N'),
         (["#....#",
           "#....#",
           ".#..#.",
           ".#..#.",
           "..##..",
           "..##..",
           ".#..#.",
           ".#..#.",
           "#....#",
           "#....#"
          ],'X'),
         (["######",
           ".....#",
           ".....#",
           "....#.",
           "...#..",
           "..#...",
           ".#....",
           "#.....",
           "#.....",
           "######"
          ],'Z')
        ]

result height points
  | height == 8  = ocr5x8 img
  | height == 10 = ocr6x10 img
  where
    img = show2ds False $ toImg points $ findT height points

result2 = findT
