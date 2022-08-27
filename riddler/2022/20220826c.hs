import Data.List(permutations)

toGroups :: Ord a => [a] -> [[a]]
toGroups [] = []
toGroups (a:as) = groups a [a] as
  where
    groups a g [] = [reverse g]
    groups a g (b:bs) | b < a = groups a (b:g) bs
                      | otherwise = reverse g : groups b [b] bs

addLane :: Ord a => [[a]] -> ([[a]],[[a]])
addLane as = (map (take 1) as,toGroups (concatMap (drop 1) as))

totalGroups1 :: Ord a => [a] -> Int
totalGroups1 as = (addGroups . addLane . toGroups) as
  where
    addGroups (lane1,lane2) = length lane1 + length lane2

totalGroups2 :: Ord a => [a] -> Int
totalGroups2 as = countGroups Nothing Nothing as
  where
    countGroups Nothing Nothing [] = 0
    countGroups (Just (_,n1)) Nothing [] = n1
    countGroups (Just (_,n1)) (Just (_,n2)) [] = n1+n2
    countGroups Nothing Nothing (a:as) = countGroups (Just (a,1)) Nothing as
    countGroups (Just (a1,n1)) Nothing (a:as)
      | a > a1 = countGroups (Just (a,n1+1)) Nothing as
      | otherwise = countGroups (Just (a1,n1)) (Just (a,1)) as
    countGroups (Just (a1,n1)) (Just (a2,n2)) (a:as)
      | a > a1 = countGroups (Just (a,n1+1)) (Just (a2,n2)) as
      | a > a2 = countGroups (Just (a1,n1)) (Just (a,n2+1)) as
      | otherwise = countGroups (Just (a1,n1)) (Just (a2,n2)) as

averageGroups :: ([Int] -> Int) -> Int -> Rational
averageGroups countGroups n = (avg . map countGroups . permutations) [1..n]
  where
    avg list = fromIntegral (sum list) / fromIntegral (length list)

avgGroups :: [(Int,Rational)]
avgGroups = iterate addCar (1,1)
  where
    addCar (n,avgNGroups) = (n+1,avgNGroups + sum [1/(fromIntegral (n+1)*product [1..i-1]) | i <- [1..fromIntegral (n+1)]])

main :: IO ()
main = do
    mapM_ print [(n,averageGroups totalGroups1 n) | n <- [1..10]]
    mapM_ print [(n,averageGroups totalGroups2 n) | n <- [1..10]]
    mapM_ print (take 20 avgGroups)
