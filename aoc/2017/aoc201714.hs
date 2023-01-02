{-
--- Day 14: Disk Defragmentation ---

Suddenly, a scheduled job activates the system's disk defragmenter. Were the
situation different, you might sit and watch it for a while, but today, you
just don't have that kind of time. It's soaking up valuable system resources
that are needed elsewhere, and so the only option is to help it finish its task
as soon as possible.

The disk in question consists of a 128x128 grid; each square of the grid is
either free or used. On this disk, the state of the grid is tracked by the bits
in a sequence of knot hashes.

A total of 128 knot hashes are calculated, each corresponding to a single row
in the grid; each hash contains 128 bits which correspond to individual grid
squares. Each bit of a hash indicates whether that square is free (0) or used (1).

The hash inputs are a key string (your puzzle input), a dash, and a number from 0 to 127 corresponding to the row. For example, if your key string were flqrgnkx, then the first row would be given by the bits of the knot hash of flqrgnkx-0, the second row from the bits of the knot hash of flqrgnkx-1, and so on until the last row, flqrgnkx-127.

The output of a knot hash is traditionally represented by 32 hexadecimal digits; each of these digits correspond to 4 bits, for a total of 4 * 32 = 128 bits. To convert to bits, turn each hexadecimal digit to its equivalent binary value, high-bit first: 0 becomes 0000, 1 becomes 0001, e becomes 1110, f becomes 1111, and so on; a hash that begins with a0c2017... in hexadecimal would begin with 10100000110000100000000101110000... in binary.

Continuing this process, the first 8 rows and columns for key flqrgnkx appear as follows, using # to denote used squares, and . to denote free ones:

| ##.#.#..-->
| .#.#.#.#   
| ....#.#.   
| #.#.##.#   
| .##.#...   
| ##..#..#   
| .#...#..   
| ##.#.##.-->
| |      |   
| V      V   

In this example, 8108 squares are used across the entire 128x128 grid.

Given your actual key string, how many squares are used?
-}

-- I assume part 2 of millisecond 10, which I have not yet seen, specifies how
-- to generate the knot hash.

import Data.Bits(popCount,testBit,xor)
import Data.Char(ord)
import Data.Set(Set,difference,fromList,intersection,size,toList,union)

step :: (Int,Int,[Int]) -> Int -> (Int,Int,[Int])
step (pos,skip,list) n = (pos+n+skip,skip+1,list3++s)
  where
    (r,list2) = splitAt n list
    (s,list3) = splitAt skip (list2 ++ reverse r)

orderList :: (Int,Int,[Int]) -> [Int]
orderList (pos,_,list) = start ++ end
  where
    (end,start) = splitAt (length list - pos `mod` length list) list

marks :: Int -> [Int] -> [Int]
marks n input = orderList $ foldl step (0,0,[0..n-1]) input

knothash :: String -> [Int]
knothash s = denseHash $ marks 256 lengths
  where
    lengths = concat $ take 64 $ repeat $ map ord s ++ [17,31,73,47,23]
    step (pos,skip,list) n = (pos+n+skip,skip+1,list3++s)
      where
        (r,list2) = splitAt n list
        (s,list3) = splitAt (skip `mod` length list) (list2 ++ reverse r)
    orderList (pos,_,list) = start ++ end
      where
        (end,start) = splitAt (length list - pos `mod` length list) list
    marks n input = orderList $ foldl step (0,0,[0..n-1]) input
    denseHash list
      | null list = []
      | otherwise = foldr xor 0 (take 16 list) : denseHash (drop 16 list)

kh :: String -> Int -> [Int]
kh key row = knothash (key ++ "-" ++ show row)

makeHashes :: String -> [[Int]]
makeHashes key = map (kh key) [0..127]

testData :: String
testData = "flqrgnkx"

test :: ()
test
  | head (kh testData 0) /= 13*16+4 = error "a"
  | head (kh testData 1) /= 5*16+5 = error "b"
  | head (kh testData 2) /= 0*16+10 = error "c"
  | head (kh testData 3) /= 10*16+13 = error "d"
  | head (kh testData 4) /= 6*16+8 = error "e"
  | head (kh testData 5) /= 12*16+9 = error "f"
  | head (kh testData 6) /= 4*16+4 = error "g"
  | head (kh testData 7) /= 13*16+6 = error "h"
  | sum (map (sum . (map popCount)) (makeHashes testData)) /= 8108 = error "i"
  | otherwise = ()

part1 :: String -> Int
part1 key = sum $ map (sum . (map popCount)) (makeHashes key)

toGrid :: [[Int]] -> Set (Int,Int)
toGrid hashes = fromList $ concat [concat [getUsed x y byte | (x,byte) <- zip [0,8..] hash] | (y,hash) <- zip [0..] hashes]
  where
    getUsed x y byte = [(x+i,y) | i <- [0..7], testBit byte (7-i)]

getGroup :: Set (Int,Int) -> Set (Int,Int)
getGroup grid = walk (fromList [minimum grid]) (fromList [minimum grid])
  where
    walk seen current
      | size current == 0 = seen
      | otherwise = walk (seen `union` next) (next `difference` seen)
      where
        next = fromList (concatMap walk1 (toList current)) `intersection` grid
        walk1 (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

countGroups :: Set (Int,Int) -> Int
countGroups grid
  | size grid == 0 = 0
  | otherwise = 1 + countGroups (grid `difference` getGroup grid)

test2 :: ()
test2
  | size testGrid /= 8108 = error "a"
  | getGroup testGrid /= fromList [(0,0),(1,0),(1,1)] = error "b"
  | countGroups testGrid /= 1242 = error "c"
  | otherwise = ()
  where testGrid = toGrid (makeHashes testData)

part2 :: String -> Int
part2 key = countGroups $ toGrid $ makeHashes key
