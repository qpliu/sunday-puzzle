import Data.Map(Map,empty,fromList,insert,mapWithKey,member,(!))

parse :: String -> ((Int,Int),Map (Int,Int) Char)
parse = build . concatMap makeRow . zip [0..] . lines
  where
    makeRow (y,line) = zip (map (flip (,) y) [0..]) line
    build cells = (fst $ head $ filter ((== 'S') . snd) cells,fromList cells)

data Dir = N | S | E | W deriving Eq

step :: Map (Int,Int) Char -> ((Int,Int),Dir) -> Maybe ((Int,Int),Dir)
step tiles ((x,y),dir)
  | not (member newXY tiles) = Nothing
  | dir == N && tiles!newXY == '|' = Just (newXY,N)
  | dir == N && tiles!newXY == '7' = Just (newXY,W)
  | dir == N && tiles!newXY == 'F' = Just (newXY,E)
  | dir == S && tiles!newXY == '|' = Just (newXY,S)
  | dir == S && tiles!newXY == 'J' = Just (newXY,W)
  | dir == S && tiles!newXY == 'L' = Just (newXY,E)
  | dir == E && tiles!newXY == '-' = Just (newXY,E)
  | dir == E && tiles!newXY == '7' = Just (newXY,S)
  | dir == E && tiles!newXY == 'J' = Just (newXY,N)
  | dir == W && tiles!newXY == '-' = Just (newXY,W)
  | dir == W && tiles!newXY == 'F' = Just (newXY,S)
  | dir == W && tiles!newXY == 'L' = Just (newXY,N)
  | otherwise = Nothing
  where
    newXY
      | dir == N = (x,y-1)
      | dir == S = (x,y+1)
      | dir == E = (x+1,y)
      | dir == W = (x-1,y)

distances :: ((Int,Int),Map (Int,Int) Char) -> Map (Int,Int) Int
distances (start,tiles) = insert start 0 $ foldl startWalk empty [N,S,E,W]
  where
    startWalk :: Map (Int,Int) Int -> Dir -> Map (Int,Int) Int
    startWalk dists dir = walk dists 1 (step tiles (start,dir))
    walk :: Map (Int,Int) Int -> Int -> Maybe ((Int,Int),Dir) -> Map (Int,Int) Int
    walk dists dist Nothing = dists
    walk dists dist (Just (xy,dir))
      | not (member xy dists) || dists!xy > dist = walk (insert xy dist dists) (dist+1) (step tiles (xy,dir))
      | otherwise = dists

result :: String -> Int
result = maximum . distances . parse

testData :: String
testData = unlines [
    "7-F7-",
    ".FJ|7",
    "SJLL7",
    "|F--J",
    "LJ.LJ"
    ]

test :: ()
test
  | result testData /= 8 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/10.txt"

clearJunk :: ((Int,Int),Map (Int,Int) Char) -> Map (Int,Int) Char
clearJunk (start,tiles) = insert start startTile $ mapWithKey clear tiles
  where
    dists = distances (start,tiles)
    isLoop xy = member xy dists
    clear xy tile
      | isLoop xy = tile
      | otherwise = '.'
    startTile = case map (step tiles . (,) start) [N,S,E,W] of
        [Just _,Just _,Nothing,Nothing] -> '|'
        [Just _,Nothing,Just _,Nothing] -> 'L'
        [Just _,Nothing,Nothing,Just _] -> 'J'
        [Nothing,Just _,Just _,Nothing] -> 'F'
        [Nothing,Just _,Nothing,Just _] -> '7'
        [Nothing,Nothing,Just _,Just _] -> '-'

data TileState = In | Out | InBelow | InAbove deriving Eq

countIns :: Int -> TileState -> Int -> Int -> Map (Int,Int) Char -> Int
countIns count tileState x y tiles
  | member (x,y) tiles = countTile (tileState,tiles!(x,y))
  | x /= 0 = countIns count Out 0 (y+1) tiles
  | otherwise = count
  where
    countTile (In,'.') = countIns (count+1) In (x+1) y tiles
    countTile (Out,'.') = countIns count Out (x+1) y tiles
    countTile (In,'|') = countIns count Out (x+1) y tiles
    countTile (Out,'|') = countIns count In (x+1) y tiles
    countTile (In,'L') = countIns count InBelow (x+1) y tiles
    countTile (Out,'L') = countIns count InAbove (x+1) y tiles
    countTile (InAbove,'J') = countIns count Out (x+1) y tiles
    countTile (InBelow,'J') = countIns count In (x+1) y tiles
    countTile (In,'F') = countIns count InAbove (x+1) y tiles
    countTile (Out,'F') = countIns count InBelow (x+1) y tiles
    countTile (InAbove,'7') = countIns count In (x+1) y tiles
    countTile (InBelow,'7') = countIns count Out (x+1) y tiles
    countTile (InAbove,'-') = countIns count InAbove (x+1) y tiles
    countTile (InBelow,'-') = countIns count InBelow (x+1) y tiles

result2 :: String -> Int
result2 = countIns 0 Out 0 0 . clearJunk . parse

testData2 :: [String]
testData2 = [unlines [
    "...........",
    ".S-------7.",
    ".|F-----7|.",
    ".||.....||.",
    ".||.....||.",
    ".|L-7.F-J|.",
    ".|..|.|..|.",
    ".L--J.L--J.",
    "..........."
    ],unlines [
    "..........",
    ".S------7.",
    ".|F----7|.",
    ".||OOOO||.",
    ".||OOOO||.",
    ".|L-7F-J|.",
    ".|II||II|.",
    ".L--JL--J.",
    ".........."
    ],unlines [
    ".F----7F7F7F7F-7....",
    ".|F--7||||||||FJ....",
    ".||.FJ||||||||L7....",
    "FJL7L7LJLJ||LJ.L-7..",
    "L--J.L7...LJS7F-7L7.",
    "....F-J..F7FJ|L7L7L7",
    "....L7.F7||L7|.L7L7|",
    ".....|FJLJ|FJ|F7|.LJ",
    "....FJL-7.||.||||...",
    "....L---J.LJ.LJLJ..."
    ],unlines [
    "FF7FSF7F7F7F7F7F---7",
    "L|LJ||||||||||||F--J",
    "FL-7LJLJ||||||LJL-77",
    "F--JF--7||LJLJ7F7FJ-",
    "L---JF-JLJ.||-FJLJJ7",
    "|F|F-JF---7F7-L7L|7|",
    "|FFJF7L7F-JF7|JL---7",
    "7-L-JL7||F7|L7F-7F7|",
    "L.L7LFJ|||||FJL7||LJ",
    "L7JLJL-JLJLJL--JLJ.L"
    ]]

test2 :: ()
test2
  | map result2 testData2 /= [4,4,8,10] = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/10.txt"
