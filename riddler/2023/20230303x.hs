import Data.List(nub)
import Data.Map(Map,alter,empty,toList)

next :: (Int,Int) -> (Int,Int)
next (s,n) = ((s+n) `mod` 100, (n+1) `mod` 100)

table :: Map Int Int
table = (foldr tabulate empty . nub . take 10000 . iterate next) (1,2)
  where
    tabulate (n,_) tab = alter (Just . maybe 1 (+1)) n tab

main :: IO ()
main = mapM_ print (toList table)
