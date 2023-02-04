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
