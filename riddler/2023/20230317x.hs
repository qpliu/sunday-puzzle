import Data.Map(fromList,(!))

count :: Int -> Int
count n = memo!(1,1)
  where
    memo = fromList [((current,revert),f current revert) | current <- [1..n], revert <- [0..current]]
    f current revert
      | current >= n = 1
      | otherwise = memo!(current+1,revert) + sum [memo!(rev+1,rev) | rev <- [revert+1..current-1]]

main :: IO ()
main = print (count 8)
