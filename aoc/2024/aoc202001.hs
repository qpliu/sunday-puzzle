module AOC202001 where

import Data.Set(fromList,member)

import AOC

aoc = AOC {
    day="../../2020/input/01",
    aocTests=[
        AOCTest {
            testData=unlines [
                "1721",
                "979",
                "366",
                "299",
                "675",
                "1456"
                ],
            testResult=Just "514579",
            testResult2=Just "241861950"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parseInts,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result ns = toProduct $ head $ filter ((`member` set) . (2020-)) ns
  where
    set = fromList ns
    toProduct n = (2020-n)*n

result2 ns = toProduct $ head $ filter ((`member` set) . (2020-) . fst) n2s
  where
    set = fromList ns
    toProduct (s,p) = (2020-s)*p
    n2s = [(n1+n2,n1*n2) | n1 <- ns, n2 <- ns]
