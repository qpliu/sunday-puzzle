module AOC201606 where

import Data.Map(alter,empty,toList)
import Data.Tuple(swap)

import AOC

aoc = AOC {
    day="../../2016/input/06",
    aocTests=[
        AOCTest {
            testData=unlines [
                "eedadn",
                "drvtee",
                "eandsr",
                "raavrd",
                "atevrs",
                "tsrnev",
                "sdttsa",
                "rasrtv",
                "nssdts",
                "ntnada",
                "svetve",
                "tesnvt",
                "vntsnd",
                "vrdear",
                "dvrsen",
                "enarar"
                ],
            testResult=Just $ show "easter",
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

parse =
    map (map swap . toList) . foldr (zipWith collect) (repeat empty) . lines
  where collect = alter (Just . maybe 1 succ)

result = map (snd . maximum)

result2 = map (snd . minimum)
