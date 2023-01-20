lastOfMonth :: Int -> Int
lastOfMonth month
  | month == 2 = 29
  | month `elem` [1,3,5,7,8,10,12] = 31
  | otherwise = 30

dates :: [(Int,Int)]
dates = [(month,day) | month <- [1..12], day <- [1..lastOfMonth month]]

isPair :: ((Int,Int),(Int,Int)) -> Bool
isPair ((m1,d1),(m2,d2)) = ((d2 == d1+m1 || m2 == d1+m1) && (d2 == d1*m1 || m2 == d1*m1)) || ((d1 == d2+m2 || m1 == d2+m2) && (d1 == d2*m2 || m1 == d2*m2))

pairs :: [((Int,Int),(Int,Int))]
pairs = filter isPair [(d1,d2) | d1 <- dates, d2 <- dates, d2 > d1]

main :: IO ()
main = print (length pairs)
