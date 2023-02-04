import Data.Array(range)
import Data.Set(Set,empty,fromList,insert,member,size)

parse :: String -> [((Int,Int),(Int,Int))]
parse = p . words
  where
    p (a:"->":b:rest) = (read ("("++a++")"),read ("("++b++")")) : p rest
    p _ = []

horizontalOrVertical :: ((Int,Int),(Int,Int)) -> Bool
horizontalOrVertical ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

points1 :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
points1 (a,b) = range (min a b,max a b)

countOverlapped :: [((Int,Int),(Int,Int))] -> Int
countOverlapped segments = size $ fromList $ scan empty $ concatMap points1 segments
  where
    scan seen [] = []
    scan seen (point:points)
      | member point seen = point : scan seen points
      | otherwise = scan (insert point seen) points

testData :: String
testData = "0,9 -> 5,9\n8,0 -> 0,8\n9,4 -> 3,4\n2,2 -> 2,1\n7,0 -> 7,4\n6,4 -> 2,0\n0,9 -> 2,9\n3,4 -> 1,4\n0,0 -> 8,8\n5,5 -> 8,2\n"

test :: ()
test
  | (countOverlapped . filter horizontalOrVertical . parse) testData /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (countOverlapped . filter horizontalOrVertical . parse) $ readFile "input/05.txt"


points2 :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
points2 (a@(a1,a2),b@(b1,b2))
  | a1 == b1 || a2 == b2 = range (min a b,max a b)
  | (a1 < b1) == (a2 < b2) = zip (range (min a1 b1,max a1 b1)) (range (min a2 b2,max a2 b2))
  | otherwise = zip (reverse (range (min a1 b1,max a1 b1))) (range (min a2 b2,max a2 b2))

countOverlapped2 :: [((Int,Int),(Int,Int))] -> Int
countOverlapped2 segments = size $ fromList $ scan empty $ concatMap points2 segments
  where
    scan seen [] = []
    scan seen (point:points)
      | member point seen = point : scan seen points
      | otherwise = scan (insert point seen) points

test2 :: ()
test2
  | (countOverlapped2 . parse) testData /= 12 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (countOverlapped2 . parse) $ readFile "input/05.txt"
