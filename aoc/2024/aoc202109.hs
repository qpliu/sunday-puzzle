module AOC202109 where

import Data.Char(ord)
import Data.List(sort)
import Data.Map(Map,delete,findWithDefault,keys,member,toList)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2021/input/09",
    aocTests=[
        AOCTest {
            testData=unlines [
                "2199943210",
                "3987894921",
                "9856789892",
                "8767896789",
                "9899965678"
                ],
            testResult=Just "15",
            testResult2=Just "1134"
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

bot :: Int
bot = ord (pred '0')

top :: Char
top = succ '9'

risk :: Map (Int,Int) Char -> ((Int,Int),Char) -> Int
risk grid ((x,y),ch)
  | findWithDefault top (x+1,y) grid <= ch
      || findWithDefault top (x-1,y) grid <= ch
      || findWithDefault top (x,y+1) grid <= ch
      || findWithDefault top (x,y-1) grid <= ch = 0
  | otherwise = ord ch - bot

result grid = sum $ map (risk grid) $ toList grid

basins :: Map (Int,Int) a -> [Int]
basins grid
  | null grid = []
  | otherwise = fill 0 grid (take 1 $ keys grid)
  where
    fill n grid [] = n : basins grid
    fill n grid (xy@(x,y):queue)
      | not (member xy grid) = fill n grid queue
      | otherwise =
          fill (n+1) (delete xy grid) ((x+1,y):(x-1,y):(x,y+1):(x,y-1):queue)
      
result2 grid =
    product $ take 3 $ reverse $ sort $ basins $ Data.Map.filter (/= '9') grid
