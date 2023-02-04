import Data.Set(Set,empty,fromList,member,size,toList,union)

countBits :: Int -> Int
countBits 0 = 0
countBits n | even n = countBits (n `div` 2)
            | otherwise = 1 + countBits (n `div` 2)

openspace :: Int -> (Int,Int) -> Bool
openspace n (x,y) = x >= 0 && y >= 0 && even (countBits (n + x*x + 3*x + 2*x*y + y + y*y))

-- breadth-first
search :: Int -> (Int,Int) -> Int -> Set (Int,Int) -> Set (Int,Int) -> Int
search n goal nsteps visited locations
  | goal `member` locations = nsteps
  | otherwise = search n goal (nsteps+1) newVisited $ fromList $ filter (not . (`member` newVisited)) $ filter (openspace n) $ concatMap moves $ toList locations
  where
    newVisited = visited `union` locations
    moves (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

test :: ()
test
  | search 10 (7,4) 0 empty (fromList [(1,1)]) /= 11 = error "a"
  | otherwise = ()

part1 :: Int -> Int
part1 n = search n (31,39) 0 empty (fromList [(1,1)])

count :: Int -> Int -> Set (Int,Int) -> Set (Int,Int) -> Int
count n remainingSteps visited locations
  | remainingSteps <= 0 = size newVisited
  | otherwise = count n (remainingSteps-1) newVisited $ fromList $ filter (not . (`member` newVisited)) $ filter (openspace n) $ concatMap moves $ toList locations
  where
    newVisited = visited `union` locations
    moves (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

part2 :: Int -> Int
part2 n = count n 50 empty (fromList [(1,1)])
