import Data.Map(Map,fromList,(!))

prob :: Rational
prob = memo!((4,4,4,40),(0,0,0),(0,0,0))
  where
    memo :: Map ((Int,Int,Int,Int),(Int,Int,Int),(Int,Int,Int)) Rational
    memo = fromList [(((deckJ,deckQ,deckK,deckN),(aJ,aQ,aK),(bJ,bQ,bK)),p ((deckJ,deckQ,deckK,deckN),(aJ,aQ,aK),(bJ,bQ,bK))) | deckJ <- [-1..4], deckQ <- [-1..4], deckK <- [-1..4], deckN <- [-1..40], aJ <- [0..4-deckJ], aQ <- [0..4-deckQ], aK <- [0..4-deckK], bJ <- [0..4-deckJ-aJ], bQ <- [0..4-deckQ-aQ], bK <- [0..4-deckK-aK]]
    p ((deckJ,deckQ,deckK,deckN),(aJ,aQ,aK),(bJ,bQ,bK))
      | deckJ < 0 || deckQ < 0 || deckK < 0 || deckN < 0 = 0
      | n <= 0 = 0
      | aJ > 2 && aQ > 2 && aK > 2 = 1
      | bJ > 2 && bQ > 2 && bK > 2 = 1
      | aJ >= 2 && (bQ >= 2 || bK >= 2) = 0
      | aQ >= 2 && (bJ >= 2 || bK >= 2) = 0
      | aK >= 2 && (bJ >= 2 || bQ >= 2) = 0
      | otherwise =
      -- neither gets a face card
        n*(n-1)/(total*(total-1))*memo!((deckJ,deckQ,deckK,deckN-2),(aJ,aQ,aK),(bJ,bQ,bK)) +
      -- a gets jack
        j*n/(total*(total-1))*memo!((deckJ-1,deckQ,deckK,deckN-1),(aJ+1,aQ,aK),(bJ,bQ,bK)) +
      -- a gets jack, b gets jack
        j*(j-1)/(total*(total-1))*memo!((deckJ-2,deckQ,deckK,deckN-1),(aJ+1,aQ,aK),(bJ+1,bQ,bK)) +
      -- a gets queen, b gets jack
        q*j/(total*(total-1))*memo!((deckJ-1,deckQ-1,deckK,deckN),(aJ,aQ+1,aK),(bJ+1,bQ,bK)) +
      -- a gets king, b gets jack
        k*j/(total*(total-1))*memo!((deckJ-1,deckQ,deckK-1,deckN),(aJ,aQ,aK+1),(bJ+1,bQ,bK)) +
      -- a gets jack, b gets queen
        j*q/(total*(total-1))*memo!((deckJ-1,deckQ-1,deckK,deckN),(aJ+1,aQ,aK),(bJ,bQ+1,bK)) +
      -- a gets queen, b gets queen
        q*(q-1)/(total*(total-1))*memo!((deckJ,deckQ-2,deckK,deckN),(aJ,aQ+1,aK),(bJ,bQ+1,bK)) +
      -- a gets king, b gets queen
        k*q/(total*(total-1))*memo!((deckJ,deckQ-1,deckK-1,deckN),(aJ,aQ,aK+1),(bJ,bQ+1,bK)) +
      -- a gets jack, b gets king
        j*k/(total*(total-1))*memo!((deckJ-1,deckQ,deckK-1,deckN),(aJ+1,aQ,aK),(bJ,bQ,bK+1)) +
      -- a gets queen, b gets king
        q*k/(total*(total-1))*memo!((deckJ,deckQ-1,deckK-1,deckN),(aJ,aQ+1,aK),(bJ,bQ,bK+1)) +
      -- a gets king, b gets king
         k*(k-1)/(total*(total-1))*memo!((deckJ,deckQ,deckK-2,deckN),(aJ,aQ,aK+1),(bJ,bQ,bK+1)) +
      -- b gets jack
         n*j/(total*(total-1))*memo!((deckJ-1,deckQ,deckK,deckN),(aJ,aQ,aK),(bJ+1,bQ,bK)) +
      -- b gets queen
         n*q/(total*(total-1))*memo!((deckJ,deckQ-1,deckK,deckN),(aJ,aQ,aK),(bJ,bQ+1,bK)) +
      -- b gets king
         n*k/(total*(total-1))*memo!((deckJ,deckQ,deckK-1,deckN),(aJ,aQ,aK),(bJ,bQ,bK+1))
     where
       n = fromIntegral deckN
       j = fromIntegral deckJ
       q = fromIntegral deckQ
       k = fromIntegral deckK
       total = n+j+q+k

choose :: Integer -> Integer -> Integer
choose n k = product [k+1..n] `div` product [1..n-k]

hp :: Integer -> Integer -> Integer -> Integer -> Rational
hp n k draws successes = fromIntegral (choose k successes * choose (n-k) (draws-successes)) / fromIntegral (choose n draws)

main :: IO ()
main = print (p, fromRational p, 2*p, fromRational (2*p))
  where
    p = j4*(j4q4*(j4q4k4 + j4q4k3) + j4q3*(j4q3k4 + j4q3k3))
      + j3*(j3q4*(j3q4k4 + j3q4k3) + j3q3*(j3q3k4 + j3q3k3))
    j4 = hp 52 4 26 4
    j3 = hp 52 4 26 3
    j4q4 = hp 48 4 22 4
    j4q3 = hp 48 4 22 3
    j3q4 = hp 48 4 23 4
    j3q3 = hp 48 4 23 3
    j4q4k4 = hp 44 4 18 4
    j4q4k3 = hp 44 4 18 3
    j4q3k4 = hp 44 4 19 4
    j4q3k3 = hp 44 4 19 3
    j3q4k4 = hp 44 4 19 4
    j3q4k3 = hp 44 4 19 3
    j3q3k4 = hp 44 4 20 4
    j3q3k3 = hp 44 4 20 3
