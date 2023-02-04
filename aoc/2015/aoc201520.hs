import Data.List(nub,subsequences)

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

factors :: Int -> [Int]
factors n = f (take 10 primes) n []
  where
    f [] _ _ = []
    f (p:ps) n facts
      | p > n = facts
      | n `mod` p /= 0 = f ps n facts
      | otherwise = f (p:ps) (n `div` p) (p:facts)

presents :: Int -> Int
presents house = 10*(sum $ map product $ nub $ subsequences $ factors house)

test :: ()
test
  | presents 1 /= 10 = error "a"
  | presents 2 /= 30 = error "a"
  | presents 3 /= 40 = error "a"
  | presents 4 /= 70 = error "a"
  | presents 5 /= 60 = error "a"
  | presents 6 /= 120 = error "a"
  | presents 7 /= 80 = error "a"
  | presents 8 /= 150 = error "a"
  | presents 9 /= 130 = error "a"
  | otherwise = ()

part1 :: Int -> Int
part1 input = head $ filter ((>= input) . presents) [1..]

part2presents :: Int -> Int
part2presents house = 11*(sum $ filter ((>= house) . (*50)) $ map product $ nub $ subsequences $ factors house)

part2 :: Int -> Int
part2 input = head $ filter ((>= input) . part2presents) [1..]
