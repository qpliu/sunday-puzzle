cm :: Integer -> Integer
cm inches = (254*inches + 50) `div` 100

sumFactors :: Integer -> Integer
sumFactors n = s 0 1
  where
    s total m | q < m = total
              | n `mod` m /= 0 = s total (m+1)
              | q /= m = s (total + q + m) (m+1)
              | otherwise = total + q
      where q = n `div` m

main :: IO ()
main = do
    mapM_ print [i | i <- [1..50000], cm i == sumFactors i]
    mapM_ print (take 6 construct)

data QueueItem a = QueueItem ([QueueItem a] -> a)

dequeue :: [QueueItem a] -> a
dequeue [] = error "dqueue"
dequeue (QueueItem f:queue) = f queue

construct :: [Integer]
construct = search 2 (254/100 - 1) 1 [] []
  where
    search :: Integer -> Rational -> Integer -> [[Integer]] -> [QueueItem [Integer]] -> [Integer]
    search m a b sieve queue
      | a <= 0 = dequeue queue
      | any ((==m) . head) sieve = dequeue (queue ++ [QueueItem (search (m+1) a b nextSieve)])
      | length ns > 0 = ns ++ dequeue nextQueue
      | length nPerfectSquares > 0 = nPerfectSquares ++ dequeue nextQueue
      | otherwise = dequeue nextQueue
      where
        nextQueue = queue ++ [QueueItem (search (m+1) a b ([2*m,3*m..]:nextSieve)),
                              QueueItem (search (m+1) (a - 1/fromIntegral m) (b + m) nextSieve)]
        nextSieve = map (dropWhile (<= m)) sieve
        ns :: [Integer]
        ns = [n | n <- [ceiling ((fromIntegral b - 1/2)/a) .. floor ((fromIntegral b + 1/2)/a)], n > 1, n `mod` m == 0, m <= n `div` m, cm n == sumFactors n]
        nPerfectSquares :: [Integer]
        nPerfectSquares = [n | n <- [ceiling ((fromIntegral b - fromIntegral m - 1/2)/a) .. floor ((fromIntegral b - fromIntegral m + 1/2)/a)], n > 1, n `mod` m == 0, m <= n `div` m, cm n == sumFactors n]
