genCode :: Int -> Int
genCode a = a*252533 `rem` 33554393

genCoord :: (Int,Int) -> (Int,Int)
genCoord (row,col) | row <= 1 = (col+1,1) | otherwise = (row-1,col+1)

find :: (Int,Int) -> (Int,Int) -> Int -> Int
find target coord code
  | target == coord = code
  | otherwise = find target (genCoord coord) $! genCode code -- $! prevents stack overflow

test :: ()
test
  | t (1,1) /= 20151125 = error "a"
  | t (6,1) /= 33071741 = error "b"
  | t (6,2) /= 6796745  = error "c"
  | t (6,6) /= 27995004 = error "d"
  | t (1,6) /= 33511524 = error "e"
  | t (1,5) /= 10071777 = error "f"
  | otherwise = ()
  where
    t target = find target (1,1) 20151125

part1 :: Int -> Int -> Int
part1 row col = find (row,col) (1,1) 20151125
