import Data.Set(Set,empty,insert,member)

parse :: String -> ((Int,Int),Set (Int,Int))
parse = p 0 0 0 empty
  where
    p x y xmax m ('#':rest) = p (x+1) y (max xmax x) (insert (x,y) m) rest
    p x y xmax m ('\n':rest) = p 0 (y+1) xmax m rest
    p x y xmax m ('.':rest) = p (x+1) y (max xmax x) m rest
    p _ y xmax m _ = ((xmax+1,y),m)

trees :: (Int,Int) -> ((Int,Int),Set (Int,Int)) -> [(Int,Int)]
trees (dx,dy) ((w,h),m) = [((i*dx) `mod` w,i*dy) | i <- [0..h `div` dy], member ((i*dx) `mod` w,i*dy) m]

testData :: String
testData = "..##.......\n#...#...#..\n.#....#..#.\n..#.#...#.#\n.#...##..#.\n..#.##.....\n.#.#.#....#\n.#........#\n#.##...#...\n#...##....#\n.#..#...#.#\n"

test :: ()
test
  | (length . trees (3,1) . parse) testData /= 7 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . trees (3,1) . parse) $ readFile "input/03.txt"

run2 :: [(Int,Int)] -> ((Int,Int),Set (Int,Int)) -> [Int]
run2 slopes mtn = map (length . (`trees` mtn)) slopes

test2 :: ()
test2
  | (product . run2 [(1,1),(3,1),(5,1),(7,1),(1,2)] . parse) testData /= 336 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (product . run2 [(1,1),(3,1),(5,1),(7,1),(1,2)] . parse) $ readFile "input/03.txt"
