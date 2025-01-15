module AOC202107 where

import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2021/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "16,1,2,0,4,2,7,1,2,14"
                ],
            testResult=Just "37",
            testResult2=Just "168"
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

-- both the test input and my input have an even number of crabs
result crabs = sum $ map (abs . ((-) median)) crabs
  where
    median =
        (sum $ take 2 $ drop ((length crabs `div` 2) - 1) $ sort crabs) `div` 2

-- minimize sum(abs(x-a[i])*(abs(x-a[i])+1)/2)
--          sum(x^2-2*x*a[i]+a[i]^2+abs(x-a[i]))/2
--  x=sum(a[i]/n)±1/2
result2 crabs =
    minimum [sum $ map (fuel . abs . ((-) (mean+i))) crabs | i <- [-1,0,1]]
  where
    mean = sum crabs `div` length crabs
    fuel dx = (dx*(dx+1)) `div` 2
