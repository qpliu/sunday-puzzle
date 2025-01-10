module AOC202220 where

import Data.Set(Set,delete,elemAt,elems,findIndex,fromList,insert,size)
import qualified Data.Set
import Data.Map(Map,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2022/input/20",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1",
                "2",
                "-3",
                "3",
                "-2",
                "0",
                "4"
                ],
            testResult=Just "3",
            testResult2=Just "1623178306"
            }
        ],
    aocCode=Code {
        codeParse=parse 1,
        codeParse2=parse 811589153,
        codeTest=result 1,
        codeTest2=result 10,
        codeResult=result 1,
        codeResult2=result 10
        }
    }

parse decryptionKey input = (numbers,(indices,sorted))
  where
    numbers = zip [0..] $ map (* decryptionKey) $ parseInts input
    indices = Data.Map.fromList $ zip numbers [0..]
    sorted = Data.Set.fromList $ zip [0..] numbers

move :: (Map (Int,Int) Rational,Set (Rational,(Int,Int))) -> (Int,Int)
     -> (Map (Int,Int) Rational,Set (Rational,(Int,Int)))
move (indices,sorted) number
  | snd number == 0 = (indices,sorted)
  | otherwise =
        (Data.Map.insert number newIndex indices,
         insert (newIndex,number) $ delete (index,number) sorted)
  where
    n = size sorted
    index = indices!number
    from = findIndex (index,number) sorted
    to1 = (from + snd number - 1) `mod` (n-1)
    to2 = (from + snd number) `mod` (n-1)
    (index1,_) = elemAt (to1 + max 0 (signum (to1+1-from))) sorted
    (index2,_) = elemAt (to2 + max 0 (signum (to2+1-from))) sorted
    newIndex
      | index1 > index2 = index1 + 1
      | otherwise = (index1+index2)/2

mix :: [(Int,Int)] -> (Map (Int,Int) Rational,Set (Rational,(Int,Int)))
    -> (Map (Int,Int) Rational,Set (Rational,(Int,Int)))
mix numbers file = foldl move file numbers

coordinates :: [Int] -> (Map (Int,Int) Rational,Set (Rational,(Int,Int)))
            -> [Int]
coordinates offsets (indices,sorted) = map getCoordinate offsets
  where
    n = size sorted
    [zero] = filter ((== 0) . snd . snd) $ elems sorted
    zeroIndex = findIndex zero sorted
    getCoordinate i = snd $ snd $ elemAt ((i + zeroIndex) `mod` n) sorted

result mixCount (numbers,file) =
    sum $ coordinates [1000,2000,3000] $ head $ drop mixCount
        $ iterate (mix numbers) file
