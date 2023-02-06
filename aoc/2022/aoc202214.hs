import Data.Set(Set,elems,fromList,insert,member,size)

parse :: String -> Set (Int,Int)
parse = fromList . concatMap (pRock . words) . lines
  where
    pRock (xy:rest) = pPath (read ("("++xy++")")) rest
    pPath (x0,y0) ("->":xy1:rest)
      | x0 /= x1 && y0 /= y1 = error (show ((x0,y0),xy1))
      | x0 < x1 = [(x,y0) | x <- [x0..x1]] ++ pPath (x1,y1) rest
      | x1 < x0 = [(x,y0) | x <- [x1..x0]] ++ pPath (x1,y1) rest
      | y0 < y1 = [(x0,y) | y <- [y0..y1]] ++ pPath (x1,y1) rest
      | y1 < y0 = [(x0,y) | y <- [y1..y0]] ++ pPath (x1,y1) rest
      where (x1,y1) = read ("("++xy1++")")
    pPath _ _ = []

getRockYMax :: Set (Int,Int) -> (Int,Int,Set (Int,Int))
getRockYMax cave = (size cave,maximum $ map snd $ elems cave,cave)

pour :: [(Int,Int)] -> (Int,Int,Set (Int,Int)) -> (Int,Set (Int,Int))
pour path@((x,y):backtrack) (rock,ymax,cave)
  | y >= ymax = (rock,cave)
  | not (member (x,y+1) cave) = pour ((x,y+1):path) (rock,ymax,cave)
  | not (member (x-1,y+1) cave) = pour ((x-1,y+1):path) (rock,ymax,cave)
  | not (member (x+1,y+1) cave) = pour ((x+1,y+1):path) (rock,ymax,cave)
  | otherwise = pour backtrack (rock,ymax,insert (x,y) cave)

testData :: String
testData = unlines [
    "498,4 -> 498,6 -> 496,6",
    "503,4 -> 502,4 -> 502,9 -> 494,9"
    ]

test :: ()
test
  | (uncurry (flip (-)) . fmap size . pour [(500,0)] . getRockYMax . parse) testData /= 24 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (flip (-)) . fmap size . pour [(500,0)] . getRockYMax . parse) $ readFile "input/14.txt"

pour2 :: [(Int,Int)] -> (Int,Int,Set (Int,Int)) -> (Int,Set (Int,Int))
pour2 path@((x,y):backtrack) (rock,ymax,cave)
  | y >= ymax+1 = pour2 backtrack (rock,ymax,insert (x,y) cave)
  | not (member (x,y+1) cave) = pour2 ((x,y+1):path) (rock,ymax,cave)
  | not (member (x-1,y+1) cave) = pour2 ((x-1,y+1):path) (rock,ymax,cave)
  | not (member (x+1,y+1) cave) = pour2 ((x+1,y+1):path) (rock,ymax,cave)
  | otherwise = pour2 backtrack (rock,ymax,insert (x,y) cave)
pour2 [] (rock,ymax,cave) = (rock,cave)

test2 :: ()
test2
  | (uncurry (flip (-)) . fmap size . pour2 [(500,0)] . getRockYMax . parse) testData /= 93 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (flip (-)) . fmap size . pour2 [(500,0)] . getRockYMax . parse) $ readFile "input/14.txt"
