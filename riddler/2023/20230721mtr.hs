search :: Int -> [String] -> [Int] -> [[String]]
search target steps digits =
    concat [try x y (newdigits i j) | (i,x) <- dgts, (j,y) <- dgts, i /= j]
  where
    dgts = zip [1..] digits
    newdigits i j = [x | (k,x) <- dgts, k /= i, k /= j]
    try x y remainingdigits
      | x+y == target = [xysteps "+"]
      | x-y == target = [xysteps "-"]
      | y-x == target = [yxsteps "-"]
      | x*y == target = [xysteps "×"]
      | y > 0 && x `mod` y == 0 && x `div` y == target = [xysteps "÷"]
      | x > 0 && y `mod` x == 0 && y `div` x == target = [yxsteps "÷"]
      | otherwise =
          search target (xysteps "+") ((x+y):remainingdigits) ++
          (if x > y
             then search target (xysteps "-") ((x-y):remainingdigits)
             else search target (yxsteps "-") ((y-x):remainingdigits)) ++
          search target (xysteps "×") (x*y:remainingdigits) ++
          (if y > 0 && x `mod` y == 0
             then search target (xysteps "÷") ((x`div`y):remainingdigits)
             else []) ++
          (if x > 0 && y `mod` x == 0
             then search target (yxsteps "÷") ((y`div`x):remainingdigits)
             else [])
      where
        xysteps op = (show x ++ op ++ show y) : steps
        yxsteps op = (show y ++ op ++ show x) : steps

main :: IO ()
main = mapM_ print [(i,take 1 $ search i [] [1..6]) | i <- [1..300]]
