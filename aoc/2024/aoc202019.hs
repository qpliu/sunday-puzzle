module AOC202019 where

import Data.Map(fromList,(!))

import AOC

aoc = AOC {
    day="../../2020/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "0: 4 1 5",
                "1: 2 3 | 3 2",
                "2: 4 4 | 5 5",
                "3: 4 5 | 5 4",
                "4: \"a\"",
                "5: \"b\"",
                "",
                "ababbb",
                "bababa",
                "abbbab",
                "aaabbb",
                "aaaabbb"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "42: 9 14 | 10 1",
                "9: 14 27 | 1 26",
                "10: 23 14 | 28 1",
                "1: \"a\"",
                "11: 42 31",
                "5: 1 14 | 15 1",
                "19: 14 1 | 14 14",
                "12: 24 14 | 19 1",
                "16: 15 1 | 14 14",
                "31: 14 17 | 1 13",
                "6: 14 14 | 1 14",
                "2: 1 24 | 14 4",
                "0: 8 11",
                "13: 14 3 | 1 12",
                "15: 1 | 14",
                "17: 14 2 | 1 7",
                "23: 25 1 | 22 14",
                "28: 16 1",
                "4: 1 1",
                "20: 14 14 | 1 15",
                "3: 5 14 | 16 1",
                "27: 1 6 | 14 18",
                "14: \"b\"",
                "21: 14 1 | 1 14",
                "25: 1 1 | 1 14",
                "22: 14 14",
                "8: 42",
                "26: 14 22 | 1 20",
                "18: 15 15",
                "7: 14 5 | 1 21",
                "24: 14 1",
                "",
                "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa",
                "bbabbbbaabaabba",
                "babbbbaabbbbbabbbbbbaabaaabaaa",
                "aaabbbbbbaaaabaababaabababbabaaabbababababaaa",
                "bbbbbbbaaaabbbbaaabbabaaa",
                "bbbababbbbaaaaaaaabbababaaababaabab",
                "ababaaaaaabaaab",
                "ababaaaaabbbaba",
                "baabbaaaabbaaaababbaababb",
                "abbbbabbbbaaaababbbbbbaaaababb",
                "aaaaabbaabaaaaababaa",
                "aaaabbaaaabbaaa",
                "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa",
                "babaaabbbaaabaababbaabababaaab",
                "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
                ],
            testResult=Just "3",
            testResult2=Just "12"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const (parse replace),
        pcodeParse2=const (parse replace2),
        pcodeTest=result,
        pcodeTest2=result,
        pcodeResult=result,
        pcodeResult2=result
        }
    }

parse :: (String -> [String] -> [String]) -> String -> (String -> Int,[String])
parse replaceRule input = (rule,messages)
  where
    [rules,messages] = parseBlankLineSeparated input

    rule message
      | elem "" $ (table!"0") message = 1
      | otherwise = 0

    table = fromList $ map (makeRule . words) rules

    makeRule [name,['"',ch,'"']] = (init name,makeCharRule ch)
    makeRule (name:subrules) =
        (init name, makeSubrules $ replaceRule (init name) subrules)

    makeCharRule ch [] = []
    makeCharRule ch (c:cs) | c == ch = [cs] | otherwise = []

    makeSubrules subrules str = r subrules [str]
      where
        r [] matches = matches
        r ("|":subrules) matches = matches ++ r subrules [str]
        r (subrule:subrules) matches =
            r subrules (concatMap (table!subrule) matches)

replace :: String -> [String] -> [String]
replace rule subrules = subrules

replace2 :: String -> [String] -> [String]
replace2 "8" subrules = ["42","|","42","8"]
replace2 "11" subrules = ["42","31","|","42","11","31"]
replace2 rule subrules = subrules

result ncpu (rule,messages) = parallelMapReduce ncpu rule sum messages
