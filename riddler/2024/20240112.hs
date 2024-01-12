import Data.List(sort)

dice :: [Int]
dice = [4,6,8,10,12,20]

rollSame :: Int -> Int -> Rational
rollSame sides1 sides2 = 1/fromIntegral (max sides1 sides2)

probSame :: Rational
probSame = sum [rollSame s1 s2 | s1 <- dice, s2 <- dice]/(fromIntegral (length dice))^2

nDistinct :: Int -> Int -> Int -> Rational
nDistinct sides1 sides2 sides3 = sum [nDist r1 r2 r3 | r1 <- [1..s1], r2 <- [1..s2], r3 <- [1..s3]]/fromIntegral (s1*s2*s3)
  where
    [s1,s2,s3] = sort [sides1,sides2,sides3]
    nDist r1 r2 r3
      | r1 == r2 && r1 == r3 = 1
      | r1 == r2 || r1 == r3 || r2 == r3 = 2
      | otherwise = 3

expDistinct :: Rational
expDistinct = sum [nDistinct s1 s2 s3 | s1 <- dice, s2 <- dice, s3 <- dice]/(fromIntegral (length dice))^3
