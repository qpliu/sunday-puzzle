import Data.Set(Set,delete,fromList,member,toList)
import qualified Data.Set

clean :: Set Int -> Set Int
clean checks = Data.Set.filter usable checks
  where
    usable n = any usableWithN checks
      where
        usableWithN m = m /= n && (m `mod` n == 0 || n `mod` m == 0)

choices :: Set Int -> [(Int,[Int])]
choices checks = filter (not . null . snd) [(n,[m | m <- toList checks, m /= n, n `mod` m == 0]) | n <- toList checks]

choose :: (Int,[Int]) -> Set Int -> Set Int
choose (take,tax) checks = foldr delete checks (take:tax)

bruteForce :: Int -> (Int,[(Int,[Int])])
bruteForce n = startSearch (fromList [1..n])
  where
    startSearch checks
      | null options = (0,[])
      | otherwise = (take+t,choice:c)
      where
        options = choices checks
        choice@(take,_) = maximum [c | c@(t,tax) <- options, tax == [1]]
        (t,c) = search (clean (choose choice checks))
    search checks
      | null options = (0,[])
      | otherwise = maximum (map recurse options)
      where
        options = [c | c@(_,tax) <- choices checks, length tax < 3]
        recurse choice@(take,_) = (take+t,choice:c)
          where
            (t,c) = search (clean (choose choice checks))

main :: IO ()
main = mapM_ print [let (take,steps) = bruteForce n in (n,take,fromIntegral take / fromIntegral (sum [1..n]) :: Double,steps) | n <- [1..25]]
