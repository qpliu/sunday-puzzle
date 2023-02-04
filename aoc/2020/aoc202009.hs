import Data.Set(Set,empty,insert,member)

xmas :: Int -> [(Int,Set Int)] -> [Int] -> [(Int,Bool)]
xmas preamble sums [] = []
xmas preamble sums (n:rest) = (n,valid) : xmas preamble ((n,empty):take (preamble-1) (map update sums)) rest
  where
    valid = any (member n . snd) sums
    update (d,dsums)
      | d == n = (d,dsums)
      | otherwise = (d,insert (d+n) dsums)

firstInvalid :: Int -> [Int] -> Int
firstInvalid preamble numbers = fst $ head $ filter (not . snd) $ drop preamble $ xmas preamble [] numbers

testData ::[Int]
testData = [35,20,15,25,47,40,62,55,65,95,102,117,150,182,127,219,299,277,309,576]

test :: ()
test
  | firstInvalid 5 testData /= 127 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (firstInvalid 25 . map read . words) $ readFile "input/09.txt"

weakness :: Int -> [Int] -> ([Int],Int) -> ((Int,Int),[Int])
weakness target numbers@(n:ns) (list,total)
  | total == target = ((minimum list,maximum list),list)
  | total > target = weakness target numbers (init list,total - last list)
  | otherwise = weakness target ns (n:list,total + n)

test2 :: ()
test2
  | fst (weakness (firstInvalid 5 testData) testData ([],0)) /= (15,47) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = do
    list <- fmap (map read . words) $ readFile "input/09.txt"
    return $ uncurry (+) $ fst $ weakness (firstInvalid 25 list) list ([],0)
