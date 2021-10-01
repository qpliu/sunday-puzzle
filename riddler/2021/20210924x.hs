intersections :: [(Integer,Integer)]
intersections = i 0 2 [(1,0)]
  where
    i lastr2 nextr0 [] = (nextr0,0) : i (nextr0^2) (nextr0+1) [(nextr0,1)]
    i lastr2 nextr0 ((x,y):xys)
      | nextr0^2 <= x^2 + y^2 =
          (nextr0,0) : i (nextr0^2) (nextr0+1) (insert (nextr0,1) ((x,y):xys))
      | lastr2 >= x^2 + y^2 = i lastr2 nextr0 (insert (x,y+1) xys)
      | otherwise = (x,y) : i (x^2 + y^2) nextr0 (insert (x,y+1) xys)
    insert (x,y) xys | y > x = xys | null xys = [(x,y)]
    insert (x,y) ((x1,y1):xys)
      | x^2 + y^2 < x1^2 + y1^2 = (x,y):(x1,y1):xys
      | x^2 + y^2 == x1^2 + y1^2 = (x,y):xys
      | otherwise = (x1,y1):insert (x,y) xys

contains :: Integer -> Integer
contains r2 =
    -- extravagantly inefficent but simple way to do this calculation
    1 + 4 * sum [if x^2 < r2 then 1 else 0 | x <- [1,3..r2]]
      + 4 * sum [if 2*x^2 < r2 then 1 else 0 | x <- [1,3..r2]]
      + 8 * sum [if x^2 + y^2 < r2 then 1 else 0 | x <- [1,3..r2], y <- [1,3..x-1]]
