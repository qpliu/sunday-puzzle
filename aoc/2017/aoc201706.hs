import Data.Map(Map)
import qualified Data.Map
import Data.Set(Set,empty,insert,member)

redistribute :: [Int] -> [Int]
redistribute banks = map update $ zip [0..] banks
  where
    len = length banks
    (n,negativeIndex) = maximum $ zip banks [0,-1..]
    index = -negativeIndex
    update (i,count)
      | i == index = n `div` len
      | (i - index) `mod` len <= n `mod` len = count + n `div` len + 1
      | otherwise = count + n `div` len

countSteps :: Int -> Set [Int] -> [Int] -> Int
countSteps nsteps seen banks
  | banks `member` seen = nsteps
  | otherwise = countSteps (nsteps+1) (insert banks seen) (redistribute banks)

test
  | redistribute [0,2,7,0] /= [2,4,1,2] = error "a"
  | redistribute [2,4,1,2] /= [3,1,2,3] = error "b"
  | redistribute [3,1,2,3] /= [0,2,3,4] = error "c"
  | redistribute [0,2,3,4] /= [1,3,4,1] = error "d"
  | redistribute [1,3,4,1] /= [2,4,1,2] = error "e"
  | countSteps 0 empty [0,2,7,0] /= 5 = error "f"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countSteps 0 empty . map read . words) $ readFile "input/06.txt"

countSteps2 :: Int -> Map [Int] Int -> [Int] -> (Int,Int)
countSteps2 nsteps seen banks =
    maybe next ((,) nsteps) (Data.Map.lookup banks seen)
  where
    next = countSteps2 (nsteps+1) (Data.Map.insert banks nsteps seen) (redistribute banks)

test2
  | countSteps2 0 Data.Map.empty [0,2,7,0] /= (5,1) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (-) . countSteps2 0 Data.Map.empty . map read . words) $ readFile "input/06.txt"
