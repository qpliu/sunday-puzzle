module AOC201611 where

import Data.Set(Set,delete,elems,empty,fromList,member,singleton,size,union)
import qualified Data.Set
import Data.Map(Map,adjust,insert,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2016/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "The first floor contains a hydrogen-compatible microchip and a lithium-compatible microchip.",
                "The second floor contains a hydrogen generator.",
                "The third floor contains a lithium generator.",
                "The fourth floor contains nothing relevant."
                ],
            testResult=Just "11",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result,
        codeResult=result,
        codeResult2=result
        }
    }

parseFloor :: [String] -> [Either String String]
parseFloor (part:('g':'e':'n':'e':'r':'a':'t':'o':'r':_):rest) =
    Left part : parseFloor rest
parseFloor (part:('m':'i':'c':'r':'o':'c':'h':'i':'p':_):rest) =
    Right (takeWhile (/= '-') part) : parseFloor rest
parseFloor (_:rest) = parseFloor rest
parseFloor [] = []

parse =
    Data.Map.fromList . zip [1..] . map (fromList . parseFloor . words) . lines

parse2 = adjust (union extraParts) 1 . parse
  where extraParts = fromList [Left "elerium",Right "elerium",
                               Left "dilithium",Right "dilithium"]

safe :: Set (Either String String) -> Bool
safe parts = not $ any fried $ elems parts
  where
    fried (Left isotope) = False
    fried (Right isotope)
      | member (Left isotope) parts = False
      | otherwise = any (either (const True) (const False)) $ elems parts

result :: Map Int (Set (Either String String)) -> Int
result initialFloors = finalCost
  where
    Just (finalCost,_) = astar heuristic neighbors snd done initialStates

    initialStates :: [(Int,(Int,Map Int (Set (Either String String))))]
    initialStates = [(0,(1,initialFloors))]

    done :: (Int,(Int,Map Int (Set (Either String String)))) -> Bool
    done (_,(floor,floors)) = and [null (floors!i) | i <- [1..3]]

    heuristic :: (Int,(Int,Map Int (Set (Either String String)))) -> Int
    heuristic (cost,(floor,floors)) =
        cost + 3*h (size (floors!1))
             + 2*h (size (floors!2))
             +   h (size (floors!3))
    h n = signum n + max 0 (2*(n-2))

    neighbors :: (Int,(Int,Map Int (Set (Either String String))))
              -> [(Int,(Int,Map Int (Set (Either String String))))]
    neighbors (cost,(floor,floors)) =
        [(cost+1,(destFloor,
                  insert floor srcRemain $ insert destFloor dest floors))
         | destFloor <- [floor+1],
           minFloor <= destFloor && destFloor <= 4,
           carry1 <- elems $ floors!floor,
           srcRemain1 <- [delete carry1 (floors!floor)],
           carry2 <- elems $ srcRemain1,
           carry <- [fromList [carry1,carry2]],
           safe carry,
           srcRemain <- [delete carry2 srcRemain1],
           safe srcRemain,
           dest <- [union carry (floors!destFloor)],
           safe dest] ++
        [(cost+1,(destFloor,
                  insert floor srcRemain $ insert destFloor dest floors))
         | destFloor <- [floor-1,floor+1],
           minFloor <= destFloor && destFloor <= 4,
           carry <- elems $ floors!floor,
           srcRemain <- [delete carry (floors!floor)],
           safe srcRemain,
           dest <- [Data.Set.insert carry (floors!destFloor)],
           safe dest]
      where
        minFloor
          | not (null $ floors!1) = 1
          | not (null $ floors!2) = 2
          | otherwise = 3
