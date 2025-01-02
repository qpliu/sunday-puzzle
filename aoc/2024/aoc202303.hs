module AOC202303 where

import Data.Char(isDigit,ord)
import Data.Map(Map,fromList,member,toList,(!))
import qualified Data.Map
import Data.Set(Set,elems,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2023/input/03",
    aocTests=[
        AOCTest {
            testData=unlines [
                "467..114..",
                "...*......",
                "..35..633.",
                "......#...",
                "617*......",
                ".....+.58.",
                "..592.....",
                "......755.",
                "...$.*....",
                ".664.598.."
                ],
            testResult=Just "4361",
            testResult2=Just "467835"
            }
        ],
    aocCode=Code {
        codeParse=parse . parse2d,
        codeParse2=parse . parse2d,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

makePartNumbers :: Map (Int,Int) Char -> Map (Int,Int) ((Int,Int),Int)
makePartNumbers grid = table
  where
    table = fromList $ map toNumber $ filter (isDigit . snd) $ toList grid
    toNumber ((x,y),char)
      | maybe False isDigit $ Data.Map.lookup (x-1,y) grid =
          ((x,y),table!(x-1,y))
      | otherwise = ((x,y),((x,y),getNumber (x+1,y) (ord char - ord '0')))
    getNumber (x,y) n
      | maybe False isDigit $ Data.Map.lookup (x,y) grid =
          getNumber (x+1,y) (10*n + ord (grid!(x,y)) - ord '0')
      | otherwise = n

makeParts :: Map (Int,Int) Char -> [((Int,Int),Char)]
makeParts = filter (isPart . snd) . toList
  where isPart c = c /= '.' && not (isDigit c)

parse grid = (makePartNumbers grid,makeParts grid)

getPartNumbers :: Map (Int,Int) ((Int,Int),Int) -> (Int,Int)
               -> Set ((Int,Int),Int)
getPartNumbers partNumbers (x,y) =
    Data.Set.fromList [partNumbers!(x+dx,y+dy)
                       | dx <- [-1,0,1], dy <- [-1,0,1],
                         member (x+dx,y+dy) partNumbers]

result (partNumbers,parts) =
    sum $ map snd $ elems $ unions
        $ map (getPartNumbers partNumbers . fst) parts

gearRatio :: Map (Int,Int) ((Int,Int),Int) -> ((Int,Int),Char) -> Int
gearRatio partNumbers (xy,char) =
    getGearRatio $ elems $ getPartNumbers partNumbers xy
  where
    getGearRatio [(_,pn1),(_,pn2)]
      | char /= '*' = 0
      | otherwise = pn1*pn2
    getGearRatio _ = 0

result2 (partNumbers,parts) = sum $ map (gearRatio partNumbers) $ parts
