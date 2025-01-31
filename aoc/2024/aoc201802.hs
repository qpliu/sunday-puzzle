module AOC201802 where

import Data.List(sort)
import Data.Set(empty,fromList,intersection,union)

import AOC

aoc = AOC {
    day="../../2018/input/02",
    aocTests=[
        AOCTest {
            testData = unlines [
                "abcde",
                "fghij",
                "klmno",
                "pqrst",
                "fguij",
                "axcye",
                "wvxyz"
                ],
            testResult=Nothing,
            testResult2=Just $ show "fgij"
            }
        ],
    aocCode=Code {
        codeParse=lines,
        codeParse2=lines,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

hasTwoOrThree :: String -> (Bool,Bool)
hasTwoOrThree = scan False False . sort
  where
    scan True True _ = (True,True)
    scan hasTwo hasThree [] = (hasTwo,hasThree)
    scan hasTwo hasThree (a:as)
      | length matching == 1 = scan True hasThree rest
      | length matching == 2 = scan hasTwo True rest
      | otherwise = scan hasTwo hasThree rest
      where
        (matching,rest) = span (== a) as

result = uncurry (*) . totals . map hasTwoOrThree
  where totals list = (length $ filter fst list,length $ filter snd list)

removeOne :: [a] -> [[a]]
removeOne [] = []
removeOne (a:as) = as : map (a:) (removeOne as)

result2 = scan empty . map (fromList . removeOne)
  where
    scan seen (this:rest)
      | null matching = scan (union seen this) rest
      | otherwise = minimum matching
      where matching = intersection seen this
