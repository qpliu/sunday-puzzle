module AOC201812 where

import Data.Array(Array,array,(!))
import Data.Bits(bit,clearBit,popCount,setBit,shiftL,shiftR,testBit,(.&.))
import Data.Map(Map,empty,insert,member,toList)
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2018/input/12",
    aocTests=[
        AOCTest {
            testData=unlines [
                "initial state: #..#.#..##......###...###",
                "",
                "...## => #",
                "..#.. => #",
                ".#... => #",
                ".#.#. => #",
                ".#.## => #",
                ".##.. => #",
                ".#### => #",
                "#.#.# => #",
                "#.### => #",
                "##.#. => #",
                "##.## => #",
                "###.. => #",
                "###.# => #",
                "####. => #"
                ],
            testResult=Just "325",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2 50000000000,
        codeResult=result,
        codeResult2=result2 50000000000
        }
    }

parseBits :: String -> Integer
parseBits = sum . map fst . filter ((== '#') . snd) . zip [bit i | i <- [0..]]

parseRule :: [String] -> (Integer,Bool)
parseRule [pattern,"=>",outcome] = (parseBits pattern,outcome == "#")

parse = p . map words . lines
  where
    p (["initial","state:",state]:[]:rules) =
        (array (0,31) ([(i,False) | i <- [0..31]] ++ map parseRule rules),
         (0,(length state,False,parseBits state)))

type LocationState = (Int,State)
type State = (Int,Bool,Integer)

potNumbers :: LocationState -> Int
potNumbers (location,(count,infinity,pots))
  | infinity = sum [i | i <- [min (-location-count) 0 .. location]]
             + sum [i | i <- [location+count .. max 0 (-location)]]
             + sum [location+i | i <- [0..count-1], testBit pots i]
  | otherwise = sum [location+i | i <- [0..count-1], testBit pots i]

trimLeft :: LocationState -> LocationState
trimLeft locationState@(location,(count,infinity,pots))
  | count == 0 || infinity /= testBit pots 0 = locationState
  | otherwise = trimLeft (location+1,(count-1,infinity,shiftR pots 1))

trimRight :: LocationState -> LocationState
trimRight locationState@(location,(count,infinity,pots))
  | count == 0 || infinity /= testBit pots (count-1) = locationState
  | otherwise = trimRight (location,(count-1,infinity,pots))

trim :: LocationState -> LocationState
trim = trimLeft . trimRight

extend :: LocationState -> LocationState
extend (location,(count,infinity,pots)) =
    (location-4,(count+8,infinity,
                 foldl (if infinity then setBit else clearBit) (shiftL pots 4)
                       [0,1,2,3,count+4,count+5,count+6,count+7]))

step :: Array Integer Bool -> LocationState -> LocationState
step rules (location,(count,infinity,pots)) =
    extend $ trim $ (location,(count,nextInfinity,setEdges nextPots))
  where
    nextInfinity | infinity = rules!31 | otherwise = rules!0
    setEdges p = foldl (if nextInfinity then setBit else clearBit) p
                       [0,1,count-2,count-1]

    nextPots = foldr nextBit 0 [2..count-3]
    nextBit b p
      | rules!((shiftR pots (b-2)) .&. 31) = setBit p b
      | otherwise = p

result (rules,locationState) =
    potNumbers $ head $ drop 20 $ iterate (step rules) $ extend locationState

findCycle :: Array Integer Bool -> Map State (Int,Int) -> Int -> LocationState
          -> ((Int,Int),(Int,Int),Map State (Int,Int))
findCycle rules history t (location,state)
  | member state history = ((t,location),history Data.Map.! state,history)
  | otherwise = findCycle rules (insert state (t,location) history) (t+1)
                          (step rules (location,state))

result2 n (rules,locationState) = potNumbers (loc,state) + dl*f*popCount pots
  where
    ((t1,loc1),(t0,loc0),history) =
        findCycle rules empty 0 $ extend locationState
    dt = t1-t0
    dl = loc1-loc0
    (f,r) = (n-t0) `divMod` dt
    [(state@(count,False,pots),(t,loc))] =
        filter ((== (t0+r)) . fst . snd) $ toList history
