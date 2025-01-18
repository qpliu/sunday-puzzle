module AOC202013 where

import AOC

aoc = AOC {
    day="../../2020/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "939",
                "7,13,x,x,59,x,31,19"
                ],
            testResult=Just "295",
            testResult2=Just "1068781"
            }
        ],
    aocCode=Code {
        codeParse=parseInts,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

result (t:buses) = uncurry (*) $ minimum $ map wait buses
  where wait bus = ((bus - t) `mod` bus,bus)

parse2 :: String -> [(Int,Int)]
parse2 = p . tail . words . map uncomma
  where
    uncomma ',' = ' '
    uncomma c = c
    p buses = map toStart $ filter ((/= "x") . snd) $ zip [0..] buses
    toStart (t0,busID) = ((-t0) `mod` bus,bus)
      where bus = read busID
    

result2 = fst . foldr convergences (0,1)
