module AOC202407 where

import AOC

aoc = AOC {
    day="07",
    testData=unlines [
    "190: 10 19",
    "3267: 81 40 27",
    "83: 17 5",
    "156: 15 6",
    "7290: 6 8 6 15",
    "161011: 16 10 13",
    "192: 17 8 14",
    "21037: 9 7 18 13",
    "292: 11 6 16 20"
    ],
    testResult="3749",
    testData2="",
    testResult2="11387",
    aocParse=parse,
    aocResult=result,
    aocParse2=parse,
    aocResult2=result2
    }

parse :: String -> [(Int,[Int])]
parse = map (p . words) . lines
  where p (a:as) = (read (init a),map read as)

result = sum . map fst . filter valid

valid (value,ns) = check value $ reverse ns
  where
    check value [n] = value == n
    check value (n:ns)
      | value < 0 = False
      | value `mod` n == 0 = check (value `div` n) ns || check (value - n) ns
      | otherwise = check (value - n) ns

result2 = sum . map fst . filter valid2

valid2 (value,ns) = check value $ reverse ns
  where
    check value [n] = value == n
    check value (n:ns)
      | value < 0 = False
      | value `mod` n == 0 && check (value `div` n) ns = True
      | l > 0 && drop l vstr == nstr && check (read (take l vstr)) ns = True
      | otherwise = check (value - n) ns
      where
        vstr = show value
        nstr = show n
        l = length vstr - length nstr
