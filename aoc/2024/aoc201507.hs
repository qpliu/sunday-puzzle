module AOC201507 where

import Data.Bits(complement,shiftL,shiftR,(.&.),(.|.))
import Data.Map(Map,fromList,(!))
import Data.Word(Word16)

import AOC

aoc = AOC {
    day="../../2015/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "123 -> x",
                "456 -> y",
                "x AND y -> d",
                "x OR y -> e",
                "x LSHIFT 2 -> f",
                "y RSHIFT 2 -> g",
                "NOT x -> h",
                "NOT y -> i"
                ],
            testResult=Just "507",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse Nothing,
        codeParse2=parse2,
        codeTest=result "e",
        codeTest2=undefined,
        codeResult=result "a",
        codeResult2=result "a"
        }
    }

parse :: Maybe Word16 -> String -> Map String Word16
parse b input = table
  where
    table = fromList $ map (p . maybe id override b . words) $ lines input
    override b toks | last toks == "b" = [show b,"->","b"] | otherwise = toks
    p [a,"->",v] = (v,arg a)
    p [a,op,b,"->",v] = (v,operation op (arg a) (arg b))
    p ["NOT",a,"->",v] = (v,complement (arg a))
    arg a
      | null nums = table!a
      | otherwise = fromIntegral $ head nums
      where nums = parseInts a
    operation "AND" a b = a .&. b
    operation "LSHIFT" a b = shiftL a (fromIntegral b)
    operation "RSHIFT" a b = shiftR a (fromIntegral b)
    operation "OR" a b = a .|. b

result = flip (!)

parse2 input = parse (Just ((parse Nothing input)!"a")) input
