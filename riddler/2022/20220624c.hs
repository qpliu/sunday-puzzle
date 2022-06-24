import Data.Bits(clearBit,setBit,testBit)
import Data.Map(Map,fromList,(!))

bruteForce :: Int -> Rational
bruteForce height = addGoat height 0 0
  where
    walkup :: Int -> Int -> Int -> Maybe Int
    walkup height tower preferred
      | preferred > height = Nothing
      | testBit tower preferred = walkup height tower (preferred+1)
      | otherwise = Just (setBit tower preferred)
    addGoat :: Int -> Int -> Int -> Rational
    addGoat height count tower
      | count == height = 1
      | otherwise = sum [maybe 0 (addGoat height (count+1)) (walkup height tower preferred) | preferred <- [1..height]]/fromIntegral height

memoized :: Int -> Rational
memoized height = memo!(2^height-1)
  where
    memo :: Map Int Rational
    memo = fromList [(tower,p tower) | tower <- [0..2^height-1]]
    p :: Int -> Rational
    p tower
      | tower == 0 = 1
      | otherwise = sum [(memo!(clearBit tower i))*f tower i/fromIntegral height | i <- [0..height-1], testBit tower i]
    f :: Int -> Int -> Rational
    f tower i
      | i > 0 && testBit tower (i-1) = 1 + f tower (i-1)
      | otherwise = 1

main :: IO ()
main = do
    mapM_ print [(n,bruteForce n) | n <- [1..6]]
    mapM_ print [(n,memoized n) | n <- [1..10]]
