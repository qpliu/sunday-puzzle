module AOC201610 where

import Data.List(sort)
import Data.Map(Map,alter,empty,toList,(!))
import qualified Data.Map

import AOC

aoc = AOC {
    day="../../2016/input/10",
    aocTests=[
        AOCTest {
            testData=unlines [
                "value 5 goes to bot 2",
                "bot 2 gives low to bot 1 and high to bot 0",
                "value 3 goes to bot 1",
                "bot 1 gives low to output 1 and high to bot 0",
                "bot 0 gives low to output 2 and high to output 0",
                "value 2 goes to bot 2"
                ],
            testResult=Just "0",
            testResult2=Just "30"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result [3,5],
        codeTest2=result2,
        codeResult=result [17,61],
        codeResult2=result2
        }
    }

parse :: String -> Map (Either Int Int) [Int]
parse input = chips
  where
    chips = Data.Map.map (sort . map takeChip) insns
    insns = foldr parseInsn empty $ map words $ lines input

    parseInsn ["value",value,"goes","to","bot",bot] =
        alter (Just . maybe [Left v] (Left v:)) (Left b)
      where ([v],[b]) = (parseInts value,parseInts bot)
    parseInsn [typeA,entityA,"gives","low","to",typeB,entityB,"and","high",
               "to",typeC,entityC] =
        alter (Just . maybe [Right (0,a)] (Right (0,a):)) b
            . alter (Just . maybe [Right (1,a)] (Right (1,a):)) c
      where
        a = parseEntity typeA entityA
        b = parseEntity typeB entityB
        c = parseEntity typeC entityC

    parseEntity "bot" = Left . head . parseInts
    parseEntity "output" = Right . head . parseInts

    takeChip = either id chip
      where chip (i,src) = head $ drop i $ chips!src

result target = either id id . fst . head . filter ((== target) . snd) . toList

result2 chips = sum (chips!Right 0) * sum (chips!Right 1) * sum (chips!Right 2)
