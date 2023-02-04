import Data.Map(Map,findWithDefault,fromList,insert,(!))

search :: Int -> (Int,Int)
search square | square < 1 = (0,0)
search square = right 1 (0,0)
  where
    right n (x,y)
      | n >= square = (x,y+n-square)
      | otherwise = up (n-2*x+1) (-x+1,y)
    up n (x,y)
      | n >= square = (x-n+square,y)
      | otherwise = left (n+2*(x-1)+1) (x,-y+1)
    left n (x,y)
      | n >= square = (x,y-n+square)
      | otherwise = down (n+2*x) (-x,y)
    down n (x,y)
      | n >= square = (x+n-square,y)
      | otherwise = right (n+2*y) (x,-y)
    

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = abs (x1-x2) + abs (y1-y2)

test :: ()
test
  | part1 1 /= 0 = error "a"
  | part1 12 /= 3 = error "b"
  | part1 23 /= 2 = error "c"
  | part1 1024 /= 31 = error "d"
  | otherwise = ()

part1 :: Int -> Int
part1 = dist (0,0) . search

search2 :: Int -> (Int,(Int,Int))
search2 target = up 1 (1,0) (fromList [((0,0),1)])
  where
    up n (x,y) spiral
      | val > target = (val,(x,y))
      | n > 0 = up (n-1) (x,y+1) (insert (x,y) val spiral)
      | otherwise = left (2*x-1) (x-1,y) (insert (x,y) val spiral)
      where val = sum [findWithDefault 0 xy spiral | xy <- [(x-1,y+1),(x-1,y),(x-1,y-1),(x,y-1)]]
    left n (x,y) spiral
      | val > target = (val,(x,y))
      | n > 0 = left (n-1) (x-1,y) (insert (x,y) val spiral)
      | otherwise = down (2*y-1) (x,y-1) (insert (x,y) val spiral)
      where val = sum [findWithDefault 0 xy spiral | xy <- [(x-1,y-1),(x,y-1),(x+1,y-1),(x+1,y)]]
    down n (x,y) spiral
      | val > target = (val,(x,y))
      | n > 0 = down (n-1) (x,y-1) (insert (x,y) val spiral)
      | otherwise = right (-2*x) (x+1,y) (insert (x,y) val spiral)
      where val = sum [findWithDefault 0 xy spiral | xy <- [(x+1,y+1),(x+1,y),(x+1,y-1),(x,y+1)]]
    right n (x,y) spiral
      | val > target = (val,(x,y))
      | n > 0 = right (n-1) (x+1,y) (insert (x,y) val spiral)
      | otherwise = up (-2*y) (x,y+1) (insert (x,y) val spiral)
      where val = sum [findWithDefault 0 xy spiral | xy <- [(x-1,y+1),(x,y+1),(x+1,y+1),(x-1,y)]]

test2 :: ()
test2
  | search2 1 /= (2,(1,1)) = error "a"
  | search2 2 /= (4,(0,1)) = error "a"
  | search2 4 /= (5,(-1,1)) = error "a"
  | search2 5 /= (10,(-1,0)) = error "a"
  | search2 10 /= (11,(-1,-1)) = error "a"
  | search2 11 /= (23,(0,-1)) = error "a"
  | search2 23 /= (25,(1,-1)) = error "a"
  | search2 25 /= (26,(2,-1)) = error "a"
  | search2 26 /= (54,(2,0)) = error "a"
  | search2 54 /= (57,(2,1)) = error "a"
  | search2 57 /= (59,(2,2)) = error "a"
  | search2 59 /= (122,(1,2)) = error "a"
  | search2 122 /= (133,(0,2)) = error "a"
  | search2 133 /= (142,(-1,2)) = error "a"
  | search2 142 /= (147,(-2,2)) = error "a"
  | search2 147 /= (304,(-2,1)) = error "a"
  | search2 304 /= (330,(-2,0)) = error "a"
  | search2 330 /= (351,(-2,-1)) = error "a"
  | search2 351 /= (362,(-2,-2)) = error "a"
  | search2 362 /= (747,(-1,-2)) = error "a"
  | search2 747 /= (806,(0,-2)) = error "a"
  | otherwise = ()

part2 :: Int -> Int
part2 = fst . search2
