import Data.List(subsequences)

primes :: [Int]
primes = 2:3:s (drop 1 primes) [5,7..]
  where s (p:ps) xs = h ++ s ps [x | x <- t, x `mod` p /= 0]
            where (h,(_:t)) = span (< p*p) xs

primeSets :: Int -> [[Int]]
primeSets n = f 0 [] primes
  where
    f total set (p:ps)
      | (total+p) `mod` n == 0 = (p:set) : f (total + p) (p:set) ps
      | otherwise = f (total + p) (p:set) ps

divisiblePrimeSets :: Int -> [[[Int]]]
divisiblePrimeSets n = concatMap (dividePrimeSet n) (primeSets n)

dividePrimeSet :: Int -> [Int] -> [[[Int]]]
dividePrimeSet n set = f (sum set `div` n) set
  where
    f :: Int -> [Int] -> [[[Int]]]
    f total set
      | sum set < total = []
      | sum set == total = [[set]]
      | otherwise = concatMap g $ filter ((== total) . sum) $ subsequences set
      where
        g :: [Int] -> [[[Int]]]
        g subset = map (subset:) $ f total $ filter (not . (`elem` subset)) set

main :: IO ()
main = print $ concat [map (sum . map length) $ take 1 $ divisiblePrimeSets n | n <- [1..6]]

-- https://oeis.org/A390531
