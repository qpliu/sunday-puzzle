import Data.Set(Set,elems,fromList,member)

score :: Set (Int,Int) -> Int
score set = sum [points x y | x <- [xmin..xmax], y <- [ymin..ymax], (x,y) `member` set]
  where
    xmin = minimum $ map fst $ elems set
    xmax = maximum $ map fst $ elems set
    ymin = minimum $ map snd $ elems set
    ymax = maximum $ map snd $ elems set
    points x y =
      (if (x+1,y)   `member` set then 1 else 0) +
      (if (x,  y+1) `member` set then 1 else 0) +
      (if (x+1,y+1) `member` set then 1 else 0) +
      (if (x-1,y+1) `member` set then 1 else 0)

makeSet :: Int -> Int -> Int -> Set (Int,Int)
makeSet width height count = fromList $ take count $ build width height 0 0 1
  where
    build w h x0 y1 y2
      | odd h = [(x,y1) | x <- [x0..x0+w-1]] ++ build w (h-1) x0 (y1-1) y2
      | w > 0 = [(x,y1) | x <- [x0..x0+w-1]] ++ [(x,y2) | x <- [x0..x0+w-1]] ++ build (if h > 0 then w else w-2) (h-2) (if h > 0 then x0 else x0+1) (y1-1) (y2+1)
      | otherwise = []

display :: Set (Int,Int) -> String
display set = unlines [' ':[if (x,y) `member` set then '1' else ' ' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    xmin = minimum $ map fst $ elems set
    xmax = maximum $ map fst $ elems set
    ymin = minimum $ map snd $ elems set
    ymax = maximum $ map snd $ elems set
