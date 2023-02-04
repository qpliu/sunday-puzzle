import Data.List(partition)

parse :: String -> [(Int,Int,Int,Int)]
parse = map p . lines
  where
    p s = (read w,read x,read y,read z)
      where
        (w,s1) = span (/= ',') s
        (x,s2) = span (/= ',') (tail s1)
        (y,s3) = span (/= ',') (tail s2)
        z = tail s3

dist :: (Int,Int,Int,Int) -> (Int,Int,Int,Int) -> Int
dist (w1,x1,y1,z1) (w2,x2,y2,z2) =
    abs (w1-w2) + abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

collect :: [[(Int,Int,Int,Int)]] -> (Int,Int,Int,Int) -> [[(Int,Int,Int,Int)]]
collect constels point = (point:concat near) : far
  where (near,far) = partition (any ((<= 3) . dist point)) constels

constellations :: [(Int,Int,Int,Int)] -> [[(Int,Int,Int,Int)]]
constellations = foldl collect []

testData :: [(Int,String)]
testData = [(2,"0,0,0,0\n 3,0,0,0\n 0,3,0,0\n 0,0,3,0\n 0,0,0,3\n 0,0,0,6\n 9,0,0,0\n12,0,0,0"),(4,"-1,2,2,0\n0,0,2,-2\n0,0,0,-2\n-1,2,0,0\n-2,-2,-2,2\n3,0,2,-1\n-1,3,2,2\n-1,0,-1,0\n0,2,1,-2\n3,0,0,0"),(3,"1,-1,0,1\n2,0,-1,0\n3,2,-1,0\n0,0,3,1\n0,0,-1,-1\n2,3,-2,0\n-2,2,0,0\n2,-2,0,-1\n1,-1,0,-1\n3,2,0,2"),(8,"1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2")]

test :: ()
test
  | any (uncurry (/=)) $ map (fmap (length . constellations . parse)) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . constellations . parse) $ readFile "input/25.txt"
