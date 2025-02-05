module AOC201604 where

import Data.Char(chr,isLower,ord)
import Data.List(sort)
import Data.Map(alter,empty,toList)
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2016/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "aaaaa-bbb-z-y-x-123[abxyz]",
                "a-b-c-d-e-f-g-h-987[abcde]",
                "not-a-real-room-404[oarel]",
                "totally-real-room-200[decoy]"
                ],
            testResult=Just "1514",
            testResult2=Nothing
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

removeDecoys :: String -> [(Int,String)]
removeDecoys room
  | checksum empty room = [(abs roomID,room)]
  | otherwise = []
  where
    [roomID] = parseInts room
    checksum table (c:cs)
      | isLower c = checksum (alter (Just . maybe (-1) (+ (-1))) c table) cs
      | c == '[' =
          take 5 (map snd $ sort $ map swap $ toList table) == take 5 cs
      | otherwise = checksum table cs

parse = concatMap removeDecoys . lines

result = sum . map fst

decrypt :: (Int,String) -> (String,Int)
decrypt (roomID,room) = (dc room,roomID)
  where
    dc (c:cs)
      | isLower c = chr ((ord c - ord 'a' + roomID) `mod` 26 + ord 'a') : dc cs
      | c == '-' = '-':dc cs
      | otherwise = []

result2 =
    snd . head . filter ((== "northpole-object-storage-") . fst) . map decrypt
