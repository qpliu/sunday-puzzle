import Data.Set(Set,fromList,member,size)

next :: (Int,Set (Int,Int)) -> (Int,Set (Int,Int))
next (xmax,grid) = (maximum [x | x <- [0..xmax+1], y <- [0..xmax+1], on x y], fromList [(x,y) | x <- [-xmax-1..xmax+1], y <- [-xmax-1..xmax+1], on x y])
  where
    on x y = member (x,y) grid || sum [1 | xx <- [x-1..x+1], yy <- [y-1..y+1], member (xx,yy) grid] >= 3

counts :: [((Int,Double),(Int,Int))]
counts = zip (zip [1..] [2*n*n/3+2*n+1 | n <- [1..]]) $ map (fmap size) $ iterate next (1,fromList [(0,0),(-1,0),(1,0),(0,1),(0,-1)])

main :: IO ()
main = do
  mapM_ print $ take 10 counts
