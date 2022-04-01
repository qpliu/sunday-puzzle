import Data.Map(Map,fromList,toList,(!))

reachesFrom :: Integer -> Map Integer Integer
reachesFrom limit = table
  where
    table = fromList ((1,1):[(n,from n) | n <- [2..3*limit+1]])
    from n
      | n > limit = n
      | n `mod` 2 == 0 = table!(n `div` 2)
      | otherwise = table!(3*n+1)

test :: Integer -> [(Integer,Integer)]
test limit = take 5 $ filter ((/= 1) . snd) $ toList $ reachesFrom limit

main :: IO ()
main = print (test 40000000)
