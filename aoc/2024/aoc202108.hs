module AOC202108 where

import Data.Map((!))
import qualified Data.Map
import Data.Set(Set,fromList,intersection,size)

import AOC

aoc = AOC {
    day="../../2021/input/08",
    aocTests=[
        AOCTest {
            testData=unlines [
                "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
                ],
            testResult=Just "0",
            testResult2=Just "5353"
            },
        AOCTest {
            testData=unlines [
                "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
                "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
                "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
                "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
                "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
                "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
                "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
                "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
                "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
                "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
                ],
            testResult=Just "26",
            testResult2=Just "61229"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = map (p . words) . lines
  where
   p signals = (map fromList $ take 10 signals,map fromList $ drop 11 signals)

count1478 :: [Set Char] -> Int
count1478 = length . filter ((`elem` [2,3,4,7]) . size)

result ncpu = parallelMapReduce ncpu (count1478 . snd) sum

decode :: ([Set Char],[Set Char]) -> Int
decode (signals,output) = toInt $ map (table!) output
  where
    [s1] = filter ((== 2) . size) signals
    [s4] = filter ((== 4) . size) signals
    [s7] = filter ((== 3) . size) signals
    [s8] = filter ((== 7) . size) signals
    [s2] = filter is2 signals
      where is2 s = size (intersection s s4) == 2 && s /= s1 && s /= s7
    [s5] = filter ((== 3) . size . intersection s2) signals
    [s6] = filter is6 signals
      where is6 s = size (intersection s s8) == 6
                 && size (intersection s s1) == 1
    [s9] = filter is9 signals
      where is9 s = size (intersection s s8) == 6
                 && size (intersection s s4) == 4
    [s0] = filter is0 signals
      where is0 s = size (intersection s s8) == 6 && s /= s6 && s /= s9
    [s3] = filter is3 signals
      where is3 s = size s == 5 && s /= s2 && s /= s5
    table = Data.Map.fromList [(s0,0),(s1,1),(s2,2),(s3,3),(s4,4),
                               (s5,5),(s6,6),(s7,7),(s8,8),(s9,9)]
    toInt [d1,d2,d3,d4] = d1*1000+d2*100+d3*10+d4

result2 ncpu = parallelMapReduce ncpu decode sum
