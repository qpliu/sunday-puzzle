module AOC201621 where

import Data.List(elemIndex)

import AOC

aoc = AOC {
    day="../../2016/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "swap position 4 with position 0",
                "swap letter d with letter b",
                "reverse positions 0 through 4",
                "rotate left 1 step",
                "move position 1 to position 4",
                "move position 3 to position 0",
                "rotate based on position of letter b",
                "rotate based on position of letter d"
                ],
            testResult=Just $ show "decab",
            testResult2=Nothing -- Just $ show "abcde" -- both abcde and deabc scramble to decab due to 
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result "abcde",
        codeTest2=result2 "decab",
        codeResult=result "abcdefgh",
        codeResult2=result2 "fbgdceah"
        }
    }

parseInsn :: [String] -> String -> String
parseInsn ["swap","position",posA,"with","position",posB] str =
    swapPosition i j str
  where
    [a] = parseInts posA
    [b] = parseInts posB
    i = min a b
    j = max a b
parseInsn ["swap","letter",letA,"with","letter",letB] str =
    swapPosition i j str
  where
    Just a = elemIndex (head letA) str
    Just b = elemIndex (head letB) str
    i = min a b
    j = max a b
parseInsn ["rotate","left",countX,_] str =
    rotate (x `mod` length str) str
  where
    [x] = parseInts countX
parseInsn ["rotate","right",countX,_] str =
    rotate ((-x) `mod` length str) str
  where
    [x] = parseInts countX
parseInsn ["rotate","based","on","position","of","letter",letX] str =
    rotate ((-r) `mod` length str) str
  where
    Just x = elemIndex (head letX) str
    r | x < 4 = x+1 | otherwise = x+2
parseInsn ["reverse","positions",posX,"through",posY] str =
    reversePositions x y str
  where
    [x] = parseInts posX
    [y] = parseInts posY
parseInsn ["move","position",posX,"to","position",posY] str =
    movePositions x y str
  where
    [x] = parseInts posX
    [y] = parseInts posY

swapPosition :: Int -> Int -> String -> String
swapPosition i j str =
    part1 ++ head part3 : tail part2 ++ head part2 : tail part3
  where
    (parts12,part3) = splitAt j str
    (part1,part2) = splitAt i parts12

rotate :: Int -> String -> String
rotate i str = part2 ++ part1
  where
    (part1,part2) = splitAt i str

reversePositions :: Int -> Int -> String -> String
reversePositions i j str = part1 ++ reverse part2 ++ part3
  where
    (parts12,part3) = splitAt (j+1) str
    (part1,part2) = splitAt i parts12

movePositions :: Int -> Int -> String -> String
movePositions i j str
  | i > j = part1 ++ last part2 : init part2 ++ part3
  | otherwise = part1 ++ tail part2 ++ head part2 : part3
  where
    (parts12,part3) = splitAt (1+max i j) str
    (part1,part2) = splitAt (min i j) parts12

parseInsn2 :: [String] -> String -> String
parseInsn2 ["swap","position",posA,"with","position",posB] str =
    swapPosition i j str
  where
    [a] = parseInts posA
    [b] = parseInts posB
    i = min a b
    j = max a b
parseInsn2 ["swap","letter",letA,"with","letter",letB] str =
    swapPosition i j str
  where
    Just a = elemIndex (head letA) str
    Just b = elemIndex (head letB) str
    i = min a b
    j = max a b
parseInsn2 ["rotate","left",countX,_] str =
    rotate ((-x) `mod` length str) str
  where
    [x] = parseInts countX
parseInsn2 ["rotate","right",countX,_] str =
    rotate (x `mod` length str) str
  where
    [x] = parseInts countX
parseInsn2 ["rotate","based","on","position","of","letter",letX] str =
    rotate (r `mod` length str) str
  where
    Just x = elemIndex (head letX) str
    [r] = [x-i | i <- [0..length str-1],
                 if i < 4
                   then x == (2*i+1) `mod` length str
                   else x == (2*i+2) `mod` length str]
parseInsn2 ["reverse","positions",posX,"through",posY] str =
    reversePositions x y str
  where
    [x] = parseInts posX
    [y] = parseInts posY
parseInsn2 ["move","position",posX,"to","position",posY] str =
    movePositions y x str
  where
    [x] = parseInts posX
    [y] = parseInts posY

parse = reverse . map (parseInsn . words) . lines

parse2 = map (parseInsn2 . words) . lines

result pwd = foldr ($) pwd

result2 pwd = foldr ($) pwd
