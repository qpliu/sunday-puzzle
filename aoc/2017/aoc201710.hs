{-
--- Day 10: Knot Hash ---

You come across some programs that are trying to implement a software emulation
of a hash based on knot-tying. The hash these programs are implementing isn't
very strong, but you decide to help them anyway. You make a mental note to
remind the Elves later not to invent their own cryptographic functions.

This hash function simulates tying a knot in a circle of string with 256 marks
on it. Based on the input to be hashed, the function repeatedly selects a span
of string, brings the ends together, and gives the span a half-twist to reverse
the order of the marks within it. After doing this many times, the order of the
marks is used to build the resulting hash.

|   4--5   pinch   4  5           4   1
|  /    \  5,0,1  / \/ \  twist  / \ / \
| 3      0  -->  3      0  -->  3   X   0
|  \    /         \ /\ /         \ / \ /
|   2--1           2  1           2   5

To achieve this, begin with a list of numbers from 0 to 255, a current position
which begins at 0 (the first element in the list), a skip size (which starts at
0), and a sequence of lengths (your puzzle input). Then, for each length:

 - Reverse the order of that length of elements in the list, starting with the
   element at the current position.
 - Move the current position forward by that length plus the skip size.
 - Increase the skip size by one.

The list is circular; if the current position and the length try to reverse
elements beyond the end of the list, the operation reverses using as many extra
elements as it needs from the front of the list. If the current position moves
past the end of the list, it wraps around to the front. Lengths larger than the
size of the list are invalid.

Here's an example using a smaller list:

Suppose we instead only had a circular list containing five elements, 0, 1, 2,
3, 4, and were given input lengths of 3, 4, 1, 5.

 - The list begins as [0] 1 2 3 4 (where square brackets indicate the current
   position).
 - The first length, 3, selects ([0] 1 2) 3 4 (where parentheses indicate the
   sublist to be reversed).
 - After reversing that section (0 1 2 into 2 1 0), we get ([2] 1 0) 3 4.
 - Then, the current position moves forward by the length, 3, plus the skip
   size, 0: 2 1 0 [3] 4. Finally, the skip size increases to 1.
 - The second length, 4, selects a section which wraps: 2 1) 0 ([3] 4.
 - The sublist 3 4 2 1 is reversed to form 1 2 4 3: 4 3) 0 ([1] 2.
 - The current position moves forward by the length plus the skip size, a total
   of 5, causing it not to move because it wraps around: 4 3 0 [1] 2. The skip
   size increases to 2.
 - The third length, 1, selects a sublist of a single element, and so reversing
   it has no effect.
 - The current position moves forward by the length (1) plus the skip size (2):
   4 [3] 0 1 2. The skip size increases to 3.
 - The fourth length, 5, selects every element starting with the second: 4)
   ([3] 0 1 2. Reversing this sublist (3 0 1 2 4 into 4 2 1 0 3) produces: 3)
   ([4] 2 1 0.
 - Finally, the current position moves forward by 8: 3 4 2 1 [0]. The skip size
   increases to 4.

In this example, the first two numbers in the list end up being 3 and 4; to
check the process, you can multiply them together to produce 12.

However, you should instead use the standard list size of 256 (with values 0 to
255) and the sequence of lengths in your puzzle input. Once this process is
complete, what is the result of multiplying the first two numbers in the list?
-}

import Data.Bits(xor)
import Data.Char(chr,isDigit,ord)

step :: (Int,Int,[Int]) -> Int -> (Int,Int,[Int])
step (pos,skip,list) n = (pos+n+skip,skip+1,list3++s)
  where
    (r,list2) = splitAt n list
    (s,list3) = splitAt (skip `mod` length list) (list2 ++ reverse r)

orderList :: (Int,Int,[Int]) -> [Int]
orderList (pos,_,list) = start ++ end
  where
    (end,start) = splitAt (length list - pos `mod` length list) list

marks :: Int -> [Int] -> [Int]
marks n input = orderList $ foldl step (0,0,[0..n-1]) input

test :: ()
test
  | take 2 (marks 5 $ parse "3,4,1,5") /= [3,4] = error "a"
  | marks 5 (parse "3,4,1,5") /= [3,4,2,1,0] = error "b"
  | otherwise = ()

parse :: String -> [Int]
parse s
  | null num = []
  | otherwise = read num : parse (drop 1 rest)
  where (num,rest) = span isDigit s

part1 :: IO Int
part1 = fmap (product . take 2 . marks 256 . parse) $ readFile "input/10.txt"

parse2 :: String -> [Int]
parse2 s = map ord (filter (/= '\n') s) ++ [17,31,73,47,23]

denseHash :: [Int] -> [Int]
denseHash list
  | null list = []
  | otherwise = foldr xor 0 (take 16 list) : denseHash (drop 16 list)

knothash :: String -> [Int]
knothash = denseHash . marks 256 . concat . take 64 . repeat . parse2

toHex :: [Int] -> String
toHex l = concatMap byteToHex l
  where
    byteToHex n = [nybbleToHex (n `div` 16),nybbleToHex (n `mod` 16)]
    nybbleToHex n
      | n < 10 = chr (ord '0' + n)
      | otherwise = chr (ord 'a' - 10 + n)

test2 :: ()
test2
  | parse "1,2,3" /= [1,2,3] = error "a"
  | parse2 "1,2,3" /= [49,44,50,44,51,17,31,73,47,23] = error "b"
  | parse2 "1,2,3\n" /= [49,44,50,44,51,17,31,73,47,23] = error "c"
  | toHex (knothash "") /= "a2582a3a0e66e6e86e3812dcb672a272" = error "d"
  | toHex (knothash "AoC 2017") /= "33efeb34ea91902bb2f59c9920caa6cd" = error "e"
  | toHex (knothash "1,2,3") /= "3efbe78a8d82f29979031a4aa0b16a9d" = error "f"
  | toHex (knothash "1,2,4") /= "63960835bcdc130f0b66d7ff4f6a5a8e" = error "g"
  | otherwise = ()

part2 :: IO String
part2 = fmap (toHex . knothash) $ readFile "input/10.txt"
