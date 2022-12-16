coeffs :: [(Int,Int,Int)]
coeffs = (1,0,0):(0,1,0):(0,0,1):c coeffs
  where
    c ((a1,b1,c1):coeffs@((a2,b2,c2):(a3,b3,c3):_)) = (a1+a2+a3,b1+b2+b3,c1+c2+c3):c coeffs

trib :: (Int,Int,Int) -> [Int]
trib (a,b,c) = map (\ (ca,cb,cc) -> ca*a+cb*b+cc*c) coeffs

is2023 :: (Int,Int,Int) -> Bool
is2023 start = head (dropWhile (< 2023) (trib start)) == 2023

main :: IO ()
main = mapM_ print $ filter (is2023 . snd) [(a+b+c,(a,b,c)) | c <- [1..25], b <- [1..c], a <- [1..b]]
