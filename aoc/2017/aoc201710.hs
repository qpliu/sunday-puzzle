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
