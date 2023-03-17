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

fullHouseInf :: Rational
fullHouseInf = 1/13*fromPair + 12/13*from2
  where
    fromPair = 1/13*from3ofAKind + 12/13*fromPairPlus1
    from2 = 2/13*fromPairPlus1

    from3ofAKind = 12/13*from3ofAKindPlus1
    fromPairPlus1 = 1/13*from3ofAKindPlus1 + 1/13*from2Pair

    from3ofAKindPlus1 = 1/13
    from2Pair = 2/13

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

main :: IO ()
main = mapM_ print [(n,fullHouse n,straight False n,straight True n,fullHouse n > straight False n - straight True n) | n <- [3..20]]
