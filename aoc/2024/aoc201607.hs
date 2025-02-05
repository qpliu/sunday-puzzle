module AOC201607 where

import AOC

aoc = AOC {
    day="../../2016/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "abba[mnop]qrst",
                "abcd[bddb]xyyx",
                "aaaa[qwer]tyui",
                "ioxxoj[asdfgh]zxcvbn"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "aba[bab]xyz",
                "xyx[xyx]xyx",
                "aaa[kek]eke",
                "zazbz[bzb]cdb"
                ],
            testResult=Nothing,
            testResult2=Just "3"
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

-- there are no nested brackets in my input
parse = map (segregate [] []) . lines
  where
    segregate outside inside [] = (outside,inside)
    segregate outside inside ip = segregate2 (out:outside) inside (drop 1 rest)
      where (out,rest) = span (/= '[') ip
    segregate2 outside inside [] = (outside,inside)
    segregate2 outside inside ip = segregate outside (ins:inside) (drop 1 rest)
      where (ins,rest) = span (/= ']') ip

tls :: ([String],[String]) -> Bool
tls (outside,inside) = any abba outside && not (any abba inside)

abba :: String -> Bool
abba (a:next@(b:c:d:_)) = (a == d && b == c && a /= b) || abba next
abba _ = False

result = length . filter tls

ssl :: ([String],[String]) -> Bool
ssl (outside,inside) = or [any (bab ab) inside | ab <- concatMap aba outside]

aba :: String -> [(Char,Char)]
aba (a:next@(b:c:_))
  | a == c && a /= b = (a,b) : aba next
  | otherwise = aba next
aba _ = []

bab :: (Char,Char) -> String -> Bool
bab ab@(a,b) (c:next@(d:e:_))
  | c == b && d == a && e == b = True
  | otherwise = bab ab next
bab _ _ = False

result2 = length . filter ssl
