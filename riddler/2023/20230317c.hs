import Data.List(sort)

fullHouse :: Integer -> Rational
fullHouse nsuits =
    (n-1)/(13*n-1)*fromPair + (12*n)/(13*n-1)*from2
  where
    n = fromIntegral nsuits

    fromPair = (n-2)/(13*n-2)*from3ofAKind + (12*n)/(13*n-2)*fromPairPlus1
    from2 = 2*(n-1)/(13*n-2)*fromPairPlus1

    from3ofAKind = (12*n)/(13*n-3)*from3ofAKindPlus1
    fromPairPlus1 = (n-2)/(13*n-3)*from3ofAKindPlus1 + (n-1)/(13*n-3)*from2Pair

    from3ofAKindPlus1 = (n-1)/(13*n-4)
    from2Pair = 2*(n-1)/(13*n-4)

fullHouseInf :: Rational
fullHouseInf = 1/13*fromPair + 12/13*from2
  where
    fromPair = 1/13*from3ofAKind + 12/13*fromPairPlus1
    from2 = 2/13*fromPairPlus1

    from3ofAKind = 12/13*from3ofAKindPlus1
    fromPairPlus1 = 1/13*from3ofAKindPlus1 + 1/13*from2Pair

    from3ofAKindPlus1 = 1/13
    from2Pair = 2/13

straight :: Bool -> Integer -> Rational
straight flush nsuits =
    1/13*fromA + 2/13*fromKor2 + 2/13*fromQor3 + 2/13*fromJor4 + 6/13*inside0Plus4
  where
    n = fromIntegral nsuits
    m | flush = 1
      | otherwise = n

    fromA = 8*m/(13*n-1)*inside3 -- 2, 3, 4, 5, 10, J, Q, K
    fromKor2 = 2*m/(13*n-1)*inside3 -- A or 9/6
             + 1*m/(13*n-1)*fromKQor23
             + 1*m/(13*n-1)*fromKJor24
             + 1*m/(13*n-1)*inside2Plus1 -- 10/5
    fromQor3 = 2*m/(13*n-1)*inside3 -- A or 8/7
             + 1*m/(13*n-1)*fromKQor23
             + 1*m/(13*n-1)*fromQJor34
             + 1*m/(13*n-1)*inside1Plus2 -- 10/5
             + 1*m/(13*n-1)*inside2Plus1 -- 9/6
    fromJor4 = 2*m/(13*n-1)*inside3 -- A or 7/8
             + 1*m/(13*n-1)*fromKJor24
             + 1*m/(13*n-1)*fromQJor34
             + 1*m/(13*n-1)*inside0Plus3 -- 5/10
             + 1*m/(13*n-1)*inside1Plus2 -- 9/6
             + 1*m/(13*n-1)*inside2Plus1 -- 8/7
    inside0Plus4 = 2*m/(13*n-1)*inside3
                 + 2*m/(13*n-1)*inside2Plus1
                 + 2*m/(13*n-1)*inside1Plus2
                 + 2*m/(13*n-1)*inside0Plus3

    fromKQor23 = 2*m/(13*n-2)*inside2 -- A or 9/6
               + 1*m/(13*n-2)*fromKQJor234
               + 1*m/(13*n-2)*inside1Plus1 -- 10/5
    fromKJor24 = 2*m/(13*n-2)*inside2 -- A or 9/6
               + 1*m/(13*n-2)*fromKQJor234
               + 1*m/(13*n-2)*inside1Plus1 -- 10/5
    fromQJor34 = 2*m/(13*n-2)*inside2 -- A or 8/7
               + 1*m/(13*n-2)*fromKQJor234
               + 1*m/(13*n-2)*inside0Plus2 -- 10/5
               + 1*m/(13*n-2)*inside1Plus1 -- 9/6
    inside3 = 3*m/(13*n-2)*inside2
    inside2Plus1 = 2*m/(13*n-2)*inside1Plus1
                 + 2*m/(13*n-2)*inside2
    inside1Plus2 = 1*m/(13*n-2)*inside0Plus2
                 + 2*m/(13*n-2)*inside1Plus1
                 + 2*m/(13*n-2)*inside2
    inside0Plus3 = 2*m/(13*n-2)*inside0Plus2
                 + 2*m/(13*n-2)*inside1Plus1
                 + 2*m/(13*n-2)*inside2

    fromKQJor234 = 2*m/(13*n-3)*inside1 -- A or 9/6
                 + 1*m/(13*n-3)*inside0Plus1 -- 10/5
    inside2 = 2*m/(13*n-3)*inside1
    inside1Plus1 = 1*m/(13*n-3)*inside0Plus1
                 + 2*m/(13*n-3)*inside1
    inside0Plus2 = 2*m/(13*n-3)*inside1
                 + 2*m/(13*n-3)*inside0Plus1

    inside1 = 1*m/(13*n-4)
    inside0Plus1 = 2*m/(13*n-4)

straightInf :: Rational
straightInf =
    1/13*fromA + 2/13*fromKor2 + 2/13*fromQor3 + 2/13*fromJor4 + 6/13*inside0Plus4
  where
    fromA = 8/13*inside3 -- 2, 3, 4, 5, 10, J, Q, K
    fromKor2 = 2/13*inside3 -- A or 9/6
             + 1/13*fromKQor23
             + 1/13*fromKJor24
             + 1/13*inside2Plus1 -- 10/5
    fromQor3 = 2/13*inside3 -- A or 8/7
             + 1/13*fromKQor23
             + 1/13*fromQJor34
             + 1/13*inside1Plus2 -- 10/5
             + 1/13*inside2Plus1 -- 9/6
    fromJor4 = 2/13*inside3 -- A or 7/8
             + 1/13*fromKJor24
             + 1/13*fromQJor34
             + 1/13*inside0Plus3 -- 5/10
             + 1/13*inside1Plus2 -- 9/6
             + 1/13*inside2Plus1 -- 8/7
    inside0Plus4 = 2/13*inside3
                 + 2/13*inside2Plus1
                 + 2/13*inside1Plus2
                 + 2/13*inside0Plus3

    fromKQor23 = 2/13*inside2 -- A or 9/6
               + 1/13*fromKQJor234
               + 1/13*inside1Plus1 -- 10/5
    fromKJor24 = 2/13*inside2 -- A or 9/6
               + 1/13*fromKQJor234
               + 1/13*inside1Plus1 -- 10/5
    fromQJor34 = 2/13*inside2 -- A or 8/7
               + 1/13*fromKQJor234
               + 1/13*inside0Plus2 -- 10/5
               + 1/13*inside1Plus1 -- 9/6
    inside3 = 3/13*inside2
    inside2Plus1 = 2/13*inside1Plus1
                 + 2/13*inside2
    inside1Plus2 = 1/13*inside0Plus2
                 + 2/13*inside1Plus1
                 + 2/13*inside2
    inside0Plus3 = 2/13*inside0Plus2
                 + 2/13*inside1Plus1
                 + 2/13*inside2

    fromKQJor234 = 2/13*inside1 -- A or 9/6
                 + 1/13*inside0Plus1 -- 10/5
    inside2 = 2/13*inside1
    inside1Plus1 = 1/13*inside0Plus1
                 + 2/13*inside1
    inside0Plus2 = 2/13*inside1
                 + 2/13*inside0Plus1

    inside1 = 1/13
    inside0Plus1 = 2/13

fullHouse7 :: Integer -> Rational
fullHouse7 nsuits =
    (n-1)/(13*n-1)*fromPair + (12*n)/(13*n-1)*from2
  where
    n = fromIntegral nsuits

    fromPair = (n-2)/(13*n-2)*from3ofAKind + (12*n)/(13*n-2)*fromPairPlus1
    from2 = 2*(n-1)/(13*n-2)*fromPairPlus1 + (11*n)/(13*n-2)*from3

    from3ofAKind = (12*n)/(13*n-3)*from3ofAKindPlus1
    fromPairPlus1 = (n-2)/(13*n-3)*from3ofAKindPlus1
                  + (n-1)/(13*n-3)*from2Pair
                  + (11*n)/(13*n-3)*fromPairPlus2
    from3 = 3*(n-1)/(13*n-3)*fromPairPlus2
          + (10*n)/(13*n-3)*from4

    from3ofAKindPlus1 = (n-1)/(13*n-4)
                      + (11*n)/(13*n-4)*from3ofAKindPlus2
    from2Pair = 2*(n-1)/(13*n-4)
              + (11*n)/(13*n-4)*from2PairPlus1
    fromPairPlus2 = (n-2)/(13*n-4)*from3ofAKindPlus2
                  + 2*(n-1)/(13*n-4)*from2PairPlus1
                  + (10*n)/(13*n-4)*fromPairPlus3
    from4 = 4*(n-1)/(13*n-4)*fromPairPlus3

    from3ofAKindPlus2 = 2*(n-1)/(13*n-5)
                      + (10*n)/(13*n-5)*from3ofAKindPlus3
    from2PairPlus1 = 2*(n-2)/(13*n-5)
                   + (n-1)/(13*n-5)*from3Pair
                   + (10*n)/(13*n-5)*from2PairPlus2
    fromPairPlus3 = (n-2)/(13*n-5)*from3ofAKindPlus3
                  + 3*(n-1)/(13*n-5)*from2PairPlus2

    from3ofAKindPlus3 = 3*(n-1)/(13*n-6)
    from3Pair = 3*(n-2)/(13*n-6)
    from2PairPlus2 = 2*(n-2)/(13*n-6)

fullHouse7Inf :: Rational
fullHouse7Inf =
    1/13*fromPair + 12/13*from2
  where
    fromPair = 1/13*from3ofAKind + 12/13*fromPairPlus1
    from2 = 2/13*fromPairPlus1 + 11/13*from3

    from3ofAKind = 12/13*from3ofAKindPlus1
    fromPairPlus1 = 1/13*from3ofAKindPlus1
                  + 1/13*from2Pair
                  + 11/13*fromPairPlus2
    from3 = 3/13*fromPairPlus2
          + 10/13*from4

    from3ofAKindPlus1 = 1/13
                      + 11/13*from3ofAKindPlus2
    from2Pair = 2/13
              + 11/13*from2PairPlus1
    fromPairPlus2 = 1/13*from3ofAKindPlus2
                  + 2/13*from2PairPlus1
                  + 10/13*fromPairPlus3
    from4 = 4/13*fromPairPlus3

    from3ofAKindPlus2 = 2/13
                      + 10/13*from3ofAKindPlus3
    from2PairPlus1 = 2/13
                   + 1/13*from3Pair
                   + 10/13*from2PairPlus2
    fromPairPlus3 = 1/13*from3ofAKindPlus3
                  + 3/13*from2PairPlus2

    from3ofAKindPlus3 = 3/13
    from3Pair = 3/13
    from2PairPlus2 = 2/13

straight7 :: Integer -> Rational
straight7 nsuits =
    1/13*sum [from1 c | c <- [1..13]]
  where
    n = fromIntegral nsuits

    from1 c1 =
        n/(13*n-1)*sum [from2 (sort [c1,c2]) | c2 <- [1..13], c2 /= c1]
      + (n-1)/(13*n-1)*from2x c1

    from2 [c1,c2] =
        n/(13*n-2)*sum [from3 (sort [c1,c2,c3]) | c3 <- [1..13], c3 /= c1 && c3 /= c2]
      + 2*(n-1)/(13*n-2)*from3x [c1,c2]
    from2x c1 =
        n/(13*n-2)*sum [from3x (sort [c1,c2]) | c2 <- [1..13], c2 /= c1]
      + (n-2)/(13*n-2)*from3xx c1

    from3 [c1,c2,c3] =
        n/(13*n-3)*sum [from4 (sort [c1,c2,c3,c4]) | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]
      + 3*(n-1)/(13*n-3)*from4x [c1,c2,c3]
    from3x [c1,c2] =
        n/(13*n-3)*sum [from4x (sort [c1,c2,c3]) | c3 <- [1..13], c3 /= c1 && c3 /= c2]
      + (2*n-3)/(13*n-3)*from4xx [c1,c2]
    from3xx c1 =
        n/(13*n-3)*sum [from4xx [c1,c2] | c2 <- [1..13], c2 /= c1]

    from4 [c1,c2,c3,c4] =
        n/(13*n-4)*sum [from5 (sort [c1,c2,c3,c4,c5]) | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]
      + (n-1)/(13*n-4)*from5x [c1,c2,c3,c4]
    from4x [c1,c2,c3] =
        n/(13*n-4)*sum [from5x (sort [c1,c2,c3,c4]) | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]
      + (3*n-4)/(13*n-4)*from5xx [c1,c2,c3]
    from4xx [c1,c2]
      | c1 > 1 && c2 > c1+4 = 0
      | c1 == 1 && c2 > 5 && c2 < 10 = 0
      | otherwise =
        11*n/(13*n-4)*sum [from5xx [c1,c2,c3] | c3 <- [1..13], c3 /= c1 && c3 /= c2]

    from5 [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise =
        n/(13*n-5)*sum [from6 [c1,c2,c3,c4,c5,c6] | c6 <- [1..13], c6 /= c1 && c6 /= c2 && c6 /= c3 && c6 /= c4 && c6 /= c5]
      + 5*(n-1)/(13*n-5)*from6x [c1,c2,c3,c4,c5]
    from5x [c1,c2,c3,c4] =
        n/(13*n-5)*sum [from6x [c1,c2,c3,c4,c5] | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]
      + (n*4-5)/(13*n-5)*from6xx [c1,c2,c3,c4]
    from5xx [c1,c2,c3]
      | c1 > 1 && c2 > c1+4 && c3 > c2+4 = 0
      | otherwise =
        n/(13*n-5)*sum [from6xx [c1,c2,c3,c4] | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]

    from6 [c1,c2,c3,c4,c5,c6]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c1 == 1 && c3 == 10 = 1
      | otherwise =
        n/(13*n-6)*sum [from7 [c1,c2,c3,c4,c5,c6,c7] | c7 <- [1..13], c7 /= c1 && c7 /= c2 && c7 /= c3 && c7 /= c4 && c7 /= c5 && c7 /= c6]
      + 6*(n-1)/(13*n-6)*from7x [c1,c2,c3,c4,c5,c6]
    from6x [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise =
        n/(13*n-6)*sum [from7x [c1,c2,c3,c4,c5,c6] | c6 <- [1..13], c6 /= c1 && c6 /= c2 && c6 /= c3 && c6 /= c4 && c6 /= c5]
      + 5*(n-1)/(13*n-6)*from7xx [c1,c2,c3,c4,c5]
    from6xx [c1,c2,c3,c4]
      | c1 > 1 && c1+2 < c2 = 0
      | c2+2 < c3 = 0
      | c3+2 < c4 = 0
      | c1 == 1 && c2 > 3 && c2 < 10 = 0
      | otherwise =
        n/(13*n-6)*sum [from7xx [c1,c2,c3,c4,c5] | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]

    from7 [c1,c2,c3,c4,c5,c6,c7]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c3 == c7 - 4 = 1
      | c1 == 1 && c4 == 10 = 1
      | otherwise = 0
    from7x [c1,c2,c3,c4,c5,c6]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c1 == 1 && c3 == 10 = 1
      | otherwise = 0
    from7xx [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise = 0

straight7Inf :: Rational
straight7Inf =
    1/13*sum [from1 c | c <- [1..13]]
  where
    from1 c1 =
        1/13*sum [from2 (sort [c1,c2]) | c2 <- [1..13], c2 /= c1]
      + 1/13*from2x c1

    from2 [c1,c2] =
        1/13*sum [from3 (sort [c1,c2,c3]) | c3 <- [1..13], c3 /= c1 && c3 /= c2]
      + 2/13*from3x [c1,c2]
    from2x c1 =
        1/13*sum [from3x (sort [c1,c2]) | c2 <- [1..13], c2 /= c1]
      + 1/13*from3xx c1

    from3 [c1,c2,c3] =
        1/13*sum [from4 (sort [c1,c2,c3,c4]) | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]
      + 3/13*from4x [c1,c2,c3]
    from3x [c1,c2] =
        1/13*sum [from4x (sort [c1,c2,c3]) | c3 <- [1..13], c3 /= c1 && c3 /= c2]
      + 2/13*from4xx [c1,c2]
    from3xx c1 =
        1/13*sum [from4xx [c1,c2] | c2 <- [1..13], c2 /= c1]

    from4 [c1,c2,c3,c4] =
        1/13*sum [from5 (sort [c1,c2,c3,c4,c5]) | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]
      + 1/13*from5x [c1,c2,c3,c4]
    from4x [c1,c2,c3] =
        1/13*sum [from5x (sort [c1,c2,c3,c4]) | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]
      + 3/13*from5xx [c1,c2,c3]
    from4xx [c1,c2]
      | c1 > 1 && c2 > c1+4 = 0
      | c1 == 1 && c2 > 5 && c2 < 10 = 0
      | otherwise =
        11/13*sum [from5xx [c1,c2,c3] | c3 <- [1..13], c3 /= c1 && c3 /= c2]

    from5 [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise =
        1/13*sum [from6 [c1,c2,c3,c4,c5,c6] | c6 <- [1..13], c6 /= c1 && c6 /= c2 && c6 /= c3 && c6 /= c4 && c6 /= c5]
      + 5/13*from6x [c1,c2,c3,c4,c5]
    from5x [c1,c2,c3,c4] =
        1/13*sum [from6x [c1,c2,c3,c4,c5] | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]
      + 4/13*from6xx [c1,c2,c3,c4]
    from5xx [c1,c2,c3]
      | c1 > 1 && c2 > c1+4 && c3 > c2+4 = 0
      | otherwise =
        1/13*sum [from6xx [c1,c2,c3,c4] | c4 <- [1..13], c4 /= c1 && c4 /= c2 && c4 /= c3]

    from6 [c1,c2,c3,c4,c5,c6]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c1 == 1 && c3 == 10 = 1
      | otherwise =
        1/13*sum [from7 [c1,c2,c3,c4,c5,c6,c7] | c7 <- [1..13], c7 /= c1 && c7 /= c2 && c7 /= c3 && c7 /= c4 && c7 /= c5 && c7 /= c6]
      + 6/13*from7x [c1,c2,c3,c4,c5,c6]
    from6x [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise =
        1/13*sum [from7x [c1,c2,c3,c4,c5,c6] | c6 <- [1..13], c6 /= c1 && c6 /= c2 && c6 /= c3 && c6 /= c4 && c6 /= c5]
      + 5/13*from7xx [c1,c2,c3,c4,c5]
    from6xx [c1,c2,c3,c4]
      | c1 > 1 && c1+2 < c2 = 0
      | c2+2 < c3 = 0
      | c3+2 < c4 = 0
      | c1 == 1 && c2 > 3 && c2 < 10 = 0
      | otherwise =
        1/13*sum [from7xx [c1,c2,c3,c4,c5] | c5 <- [1..13], c5 /= c1 && c5 /= c2 && c5 /= c3 && c5 /= c4]

    from7 [c1,c2,c3,c4,c5,c6,c7]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c3 == c7 - 4 = 1
      | c1 == 1 && c4 == 10 = 1
      | otherwise = 0
    from7x [c1,c2,c3,c4,c5,c6]
      | c1 == c5 - 4 = 1
      | c2 == c6 - 4 = 1
      | c1 == 1 && c3 == 10 = 1
      | otherwise = 0
    from7xx [c1,c2,c3,c4,c5]
      | c1 == c5 - 4 = 1
      | c1 == 1 && c2 == 10 = 1
      | otherwise = 0

main :: IO ()
main = mapM_ print [(n,fullHouse n,straight False n,straight True n,fullHouse n > straight False n - straight True n) | n <- [3..20]]
