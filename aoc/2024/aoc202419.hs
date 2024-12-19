module AOC202419 where

import Data.List(sort)
import Data.Set(elems,empty,fromList,insert,member)

import AOC

aoc = AOC {
    day="19",
    testData=unlines [
    "r, wr, b, g, bwu, rb, gb, br",
    "",
    "brwrr",
    "bggr",
    "gbbr",
    "rrbgbr",
    "ubwu",
    "bwurrg",
    "brgr",
    "bbrgwb"
    ],
    testResult="6",
    testData2="",
    testResult2="16",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse,
    aocTest2=result2,
    aocResult2=result2
    }

parse = p . lines
  where p (patterns:"":designs) = (words $ filter (/= ',') patterns,designs)

possible maxlen patterns design
  | null design = True
  | otherwise = any matches [1..l]
  where
    l = min maxlen (length design)
    matches len
      | null design = True
      | len > length design = False
      | member h patterns = possible maxlen patterns t
      | otherwise = False
      where (h,t) = splitAt len design

minPatSet pats = foldl add empty sortedByLength
  where
    add patset pat
      | possible (length pat) patset pat = patset
      | otherwise = insert pat patset
    sortedByLength = map snd $ sort $ zip (map length pats) pats

possibles (patterns,designs) = filter (possible minmaxlen minpats) designs
  where
    minpats = minPatSet patterns
    minmaxlen = maximum $ map length $ elems minpats

result = length . possibles

result2 (patterns,designs) = sum $ map (waysToMake maxlen pats) designs
  where
    pats = fromList patterns
    maxlen = maximum $ map length patterns

waysToMake maxlen pats design = w design
  where
    w [] = 1
    w des@[_]
      | member des pats = 1
      | otherwise = 0
    w des = w left*w right
          + sum [w (reverse (drop i rleft)) * w (drop j right)
                 | i <- [1 .. min maxlen (length left)],
                   j <- [1 .. min (maxlen-i) (length right)],
                   member (reverse (take i rleft) ++ take j right) pats]
      where
        (left,right) = splitAt ((length des) `div` 2) des
        rleft = reverse left
