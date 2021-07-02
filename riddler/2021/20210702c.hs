import Data.List(permutations)
import Data.Time(getCurrentTime)

joinDots :: Eq a => [a] -> a -> a -> ([a],[a])
joinDots dots a b = start dots []
  where
    start [] _ = error "Missing both"
    start (dot:dots) collected
      | dot == a = continue dots collected b []
      | dot == b = continue dots collected a []
      | otherwise = start dots (dot:collected)
    continue [] _ _ _ = error "Missing one"
    continue (dot:dots) collected target opposite
      | dot == target = (reverse dots ++ collected,opposite)
      | otherwise = continue dots collected target (dot:opposite)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs (a:as) = map ((,) a) as ++ pairs as

maxScore :: (Num a, Ord a) => [a] -> a
maxScore [] = 0
maxScore [_] = 0
maxScore [a,b] = a*b
maxScore dots =
    maximum [a*b + maxSubscores (joinDots dots a b) | (a,b) <- pairs dots]
  where
    maxSubscores (loop1,loop2) = maxScore loop1 + maxScore loop2

search :: Int -> Int -> (Int,[Int]) -> [[Int]] -> IO ()
search blocksize n best arrangements
  | null arrangements = return ()
  | otherwise = do
      getCurrentTime >>= print
      let blockbest = minimum [(maxScore dots,dots) | dots <- take blocksize arrangements]
      print (n,product [3..10],blockbest,best)
      search blocksize (n+blocksize) (min blockbest best) (drop blocksize arrangements)

main :: IO ()
main = do
    print (maxScore [1..11],250)
    print (maxScore [1,4,8,7,11,2,5,9,3,6,10],237)
    search 1000 0 (250,[]) (map (1:) (filter twobeforethree (permutations [2..11])))
  where
    twobeforethree [] = True
    twobeforethree (2:_) = True
    twobeforethree (3:_) = False
    twobeforethree (_:dots) = twobeforethree dots
