module AOC202213 where

import Data.Char(isDigit)
import Data.List(sort)

import AOC

aoc = AOC {
    day="../../2022/input/13",
    aocTests=[
        AOCTest {
            testData=unlines [
                "[1,1,3,1,1]",
                "[1,1,5,1,1]",
                "",
                "[[1],[2,3,4]]",
                "[[1],4]",
                "",
                "[9]",
                "[[8,7,6]]",
                "",
                "[[4,4],4,4]",
                "[[4,4],4,4,4]",
                "",
                "[7,7,7,7]",
                "[7,7,7]",
                "",
                "[]",
                "[3]",
                "",
                "[[[]]]",
                "[[]]",
                "",
                "[1,[2,[3,[4,[5,6,7]]]],8,9]",
                "[1,[2,[3,[4,[5,6,0]]]],8,9]"
                ],
            testResult=Just "13",
            testResult2=Just "140"
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

data Packet = L [Packet] | I Int deriving (Eq,Show)

instance Ord Packet where
    compare (I a) (I b) = compare a b
    compare (I a) (L b) = comparePacketList [I a] b
    compare (L a) (I b) = comparePacketList a [I b]
    compare (L a) (L b) = comparePacketList a b

comparePacketList :: [Packet] -> [Packet] -> Ordering
comparePacketList [] [] = EQ
comparePacketList [] (_:_) = LT
comparePacketList (_:_) [] = GT
comparePacketList (a:as) (b:bs)
  | cmp == EQ = comparePacketList as bs
  | otherwise = cmp
  where cmp = compare a b

-- there are no negative integers in my input
parse = parsePackets . dropWhile (/= '[')
  where
    parsePackets [] = []
    parsePackets input = item:parsePackets (dropWhile (/= '[') rest)
      where (item,rest) = parseItem input

    parseList items (']':input) = (L (reverse items),input)
    parseList items (',':input) = parseList (item:items) rest
      where (item,rest) = parseItem input

    parseItem ('[':']':input) = (L [],input)
    parseItem ('[':input) = parseList [] (',':input)
    parseItem input
      | isDigit $ head input = (I (read i),rest)
      | otherwise = error (show input)
      where (i,rest) = span isDigit input

countPairs :: Int -> [(Int,Packet)] -> Int
countPairs n [] = n
countPairs n ((ia,a):(ib,b):rest)
  | a <= b = countPairs (n + (ib `div` 2)) rest
  | otherwise = countPairs n rest

result = countPairs 0 . zip [1..]

result2 =
    product . map fst . filter isDiv . zip [1..] . sort . (div2:) . (div6:)
  where
    div2 = L [L [I 2]]
    div6 = L [L [I 6]]
    isDiv (_,p) = p == div2 || p == div6
