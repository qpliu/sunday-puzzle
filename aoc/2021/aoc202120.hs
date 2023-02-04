import Data.Set(Set,elems,fromList,member,size)
import qualified Data.Set

parse :: String -> (Set Int,(Set (Int,Int),Bool))
parse input = (fromList $ map fst $ filter ((== '#') . snd) $ zip [0..] algo,(fromList $ p 0 0 img,False))
  where
    (algo,img) = splitAt 513 input
    p x y [] = []
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y ('#':rest) = (x,y) : p (x+1) y rest
    p x y (_:rest) = p (x+1) y rest

getEnhanced :: Set Int -> Set (Int,Int) -> Bool -> (Int,Int) -> Bool
getEnhanced algo img reversed (x,y) = member (sum [
    if member (x-1,y-1) img /= reversed then 256 else 0,
    if member (x,  y-1) img /= reversed then 128 else 0,
    if member (x+1,y-1) img /= reversed then  64 else 0,
    if member (x-1,y)   img /= reversed then  32 else 0,
    if member (x,  y)   img /= reversed then  16 else 0,
    if member (x+1,y)   img /= reversed then   8 else 0,
    if member (x-1,y+1) img /= reversed then   4 else 0,
    if member (x,  y+1) img /= reversed then   2 else 0,
    if member (x+1,y+1) img /= reversed then   1 else 0]) algo

getEnhanceTargets :: Set (Int,Int) -> [(Int,Int)]
getEnhanceTargets = elems . fromList . concatMap getPatch . elems
  where getPatch (x,y) = [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1]]

enhance :: (Set Int,(Set (Int,Int),Bool)) -> (Set Int,(Set (Int,Int),Bool))
enhance (algo,(img,reversed)) = (algo,(fromList $ filter ((nextReversed /=) . getEnhanced algo img reversed) $ getEnhanceTargets img,nextReversed))
  where nextReversed = reversed /= (member 0 algo)

testData :: String
testData = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#\n\n#..#.\n#....\n##..#\n..#..\n..###\n"

test :: ()
test
  | (size . fst . snd . enhance . enhance . parse) testData /= 35 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (size . fst . snd . enhance . enhance . parse) $ readFile "input/20.txt"

test2 :: ()
test2
  | (size . fst . snd . head . drop 50 . iterate enhance . parse) testData /= 3351 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (size . fst . snd . head . drop 50 . iterate enhance . parse) $ readFile "input/20.txt"
