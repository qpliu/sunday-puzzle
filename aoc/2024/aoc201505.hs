module AOC201505 where

import Data.Set(Set,empty,insert,member)

import AOC

aoc = AOC {
    day="../../2015/input/05",
    aocTests=[
        AOCTest {
            testData=unlines [
                "ugknbfddgicrmopn",
                "aaa",
                "jchzalrnumimnmhp",
                "haegwjzuvuyypxyu",
                "dvszwmarrgswjxmb"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "qjhvhtzxzqqjkmpb",
                "xxyxx",
                "uurcxstgmygtbstg",
                "ieodomkazucvgmuy"
                ],
            testResult=Nothing,
            testResult2=Just "2"
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

parse = lines

nice :: Int -> Bool -> String -> Bool
nice _ _ ('a':'b':_) = False
nice _ _ ('c':'d':_) = False
nice _ _ ('p':'q':_) = False
nice _ _ ('x':'y':_) = False
nice vowels pair (a:rest@(b:_))
  | elem a "aeiou" = nice (vowels+1) (pair || a == b) rest
  | otherwise = nice vowels (pair || a == b) rest
nice vowels pair (a:rest)
  | elem a "aeiou" = nice (vowels+1) pair rest
  | otherwise = nice vowels pair rest
nice vowels pair [] = vowels >= 3 && pair

result = length . filter (nice 0 False)

nice2a :: String -> Bool
nice2a = hasPair empty
  where
    hasPair pairs (a:b:rest@(c:_))
      | member [a,b] pairs = True
      | a == b && b == c = hasPair (insert [a,b] pairs) rest
    hasPair pairs (a:rest@(b:_))
      | member [a,b] pairs = True
      | otherwise = hasPair (insert [a,b] pairs) rest
    hasPair _ _ = False

nice2b :: String -> Bool
nice2b (a:rest@(b:c:_))
  | a == c = True
  | otherwise = nice2b rest
nice2b _ = False

result2 = length . filter nice2a . filter nice2b
