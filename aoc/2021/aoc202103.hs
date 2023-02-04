import Data.Bits(setBit)

toBit :: Char -> Int
toBit '0' = 0
toBit _ = 1

collect :: [Int] -> (Int,[Int]) -> (Int,[Int])
collect bits (n,counts) = (n+1,zipWith (+) bits counts)

gammaEpsilon :: (Int,[Int]) -> (Int,Int)
gammaEpsilon (n,counts) = foldr addBit (0,0) $ zip [0..62] counts
  where
    addBit (bit,count) (gamma,epsilon)
      | 2*count > n = (setBit gamma bit,epsilon)
      | otherwise = (gamma,setBit epsilon bit)

testData :: String
testData = "00100\n11110\n10110\n10111\n10101\n01111\n00111\n11100\n10000\n11001\n00010\n01010\n"

test :: ()
test
  -- blows up with empty input
  | (gammaEpsilon . foldr collect (0,repeat 0) . map (reverse . map toBit) . words) testData /= (22,9) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . gammaEpsilon . foldr collect (0,repeat 0) . map (reverse . map toBit) . words) $ readFile "input/03.txt"

oxygenCO2 :: [[Int]] -> (Int,Int)
oxygenCO2 report = (oxygen 0 report,co2 0 report)

oxygen :: Int -> [[Int]] -> Int
oxygen bit nums
  | length nums == 1 =
      sum $ zipWith (*) (reverse (head nums)) [2^n | n <- [0..62]]
  | 2*sum (map (head . drop bit) nums) >= length nums =
      oxygen (bit+1) (filter ((== 1) . head . drop bit) nums)
   | otherwise =
      oxygen (bit+1) (filter ((== 0) . head . drop bit) nums)

co2 :: Int -> [[Int]] -> Int
co2 bit nums
  | length nums == 1 =
      sum $ zipWith (*) (reverse (head nums)) [2^n | n <- [0..62]]
  | 2*sum (map (head . drop bit) nums) >= length nums =
      co2 (bit+1) (filter ((== 0) . head . drop bit) nums)
   | otherwise =
      co2 (bit+1) (filter ((== 1) . head . drop bit) nums)

test2 :: ()
test2
  | (oxygenCO2 . map (map toBit) . words) testData /= (23,10) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (*) . oxygenCO2 . map (map toBit) . words) $ readFile "input/03.txt"
