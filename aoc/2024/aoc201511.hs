module AOC201511 where

import AOC

aoc = AOC {
    day="../../2015/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "abcdefgh"
                ],
            testResult=Just $ show "abcdffaa",
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

squash :: Char -> Char
squash c
  | c > 'o' = pred $ pred $ pred c
  | c > 'l' = pred $ pred c
  | c > 'i' = pred c
  | otherwise = c

unsquash :: Char -> Char
unsquash c
  | c > pred (pred (pred 'o')) = succ $ succ $ succ c
  | c > pred (pred 'l') = succ $ succ c
  | c > pred 'i' = succ c
  | otherwise = c

noILO :: String -> String
noILO (c:cs)
  | elem c "ilo" = succ c:map (const 'a') cs
  | otherwise = c:noILO cs
noILO [] = []

parse = map squash . reverse . noILO . filter (/= '\n')

inc :: String -> String
inc (c:rest)
  | c == 'w' = 'a':inc rest
  | otherwise = succ c:rest

valid :: String -> Bool
valid pwd
  | noStraight pwd = False
  | otherwise = twoPairs 0 'a' pwd
  where
    noStraight (a:rest@(b:rest2@(c:_)))
      | b /= succ c = noStraight rest2
      | a == succ b = False
      | otherwise = noStraight rest
    noStraight _ = True
    twoPairs n x (a:rest@(b:rest2))
      | a /= b = twoPairs n x rest
      | n == 0 = twoPairs 1 a rest2
      | a /= x = True
      | otherwise = twoPairs 1 a rest2
    twoPairs _ _ [_] = False

result =
    map unsquash . reverse . head . filter valid . iterate inc . inc

result2 =
    map unsquash . reverse . head . drop 1 . filter valid . iterate inc . inc
