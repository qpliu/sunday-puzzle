import Data.Map(Map,fromList,toList,unionsWith)

blockCount :: Int -> Int
blockCount n | n <= 1 = 1 | otherwise = n + blockCount (n-1)

addBlock :: Int -> [Int] -> [[Int]]
addBlock nstairs stairs
  | len < nstairs = (1:stairs) : up (len-1) stairs
  | otherwise = up (len-1) stairs
  where
    len = length stairs
    up l [] = []
    up l [a]
      | a < nstairs = [[a+1]]
      | otherwise = []
    up l (a:bs@(b:_))
      | a < nstairs - l && a < b = ((a+1):bs) : map (a:) (up (l-1) bs)
      | otherwise = map (a:) (up (l-1) bs)

bruteforceways :: Int -> Int
bruteforceways nstairs = length (head (drop (blockCount nstairs - 1) (iterate (concatMap (addBlock nstairs)) [[]])))

addBlockWays :: Int -> Map [Int] Integer -> Map [Int] Integer
addBlockWays nstairs ways = unionsWith (+) (map (fromList . add) (toList ways))
  where
    add (stairs,nways) = map (flip (,) nways) (addBlock nstairs stairs)

ways :: Int -> Integer
ways nstairs = sum (head (drop (blockCount nstairs) (iterate (addBlockWays nstairs) (fromList [([],1)]))))

a005118 :: Integer -> Integer
a005118 n = product [1..n*(n-1)`div`2] `div` product [(2*i+1)^(n-i-1) | i <- [1..n-2]]

main :: IO ()
main = do
    mapM_ (print . ways) [1..10]
    mapM_ (print . a005118) [2..11]
