module AOC202104 where

import Data.Set(Set,fromList,size)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2021/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1",
                "",
                "22 13 17 11  0",
                " 8  2 23  4 24",
                "21  9 14 16  7",
                " 6 10  3 18  5",
                " 1 12 20 15 19",
                "",
                " 3 15  0  2 22",
                " 9 18 13 17  5",
                "19  8  7 25 23",
                "20 11 10 24  4",
                "14 21 16 12  6",
                "",
                "14 21 17 24  4",
                "10 16 15  9 19",
                "18  8 23 26 20",
                "22 11 13  6  5",
                " 2  0 12  3  7"
                ],
            testResult=Just "4512",
            testResult2=Just "1924"
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

parse = p . span (/= '\n')
  where
    p (numbers,boards) = (parseInts numbers,parseBoards $ parseInts boards)
    boardSpots = [(x,y) | x <- [1..5], y <- [1..5]]
    parseBoards = pBoards . splitAt 25
    pBoards (boardNums,rest)
      | null rest = [board]
      | otherwise = board : parseBoards rest
      where board = fromList $ zip boardSpots boardNums

mark :: Int -> [Set ((Int,Int),Int)] -> [Set ((Int,Int),Int)]
mark n = map (Data.Set.filter ((/= n) . snd))

winner :: Set ((Int,Int),a) -> Bool
winner board = size (Data.Set.map (fst . fst) board) < 5
            || size (Data.Set.map (snd . fst) board) < 5

findWinner :: ([Int],[Set ((Int,Int),Int)]) -> Int
findWinner ((n:ns),boards)
  | null winners = findWinner (ns,markedBoards)
  | otherwise = n * sum (Data.Set.map snd (head winners))
  where
    markedBoards = mark n boards
    winners = filter winner markedBoards

result = findWinner

findLoser :: ([Int],[Set ((Int,Int),Int)]) -> Int
findLoser ((n:ns),boards)
  | null losers = n * sum (Data.Set.map snd (head markedBoards))
  | otherwise = findLoser (ns,losers)
  where
    markedBoards = mark n boards
    losers = filter (not . winner) markedBoards

result2 = findLoser
