import Debug.Trace(traceShow)
import Data.Set(Set,empty,fromList,insert,member,toList)
import Data.Map(Map,(!))
import qualified Data.Map

parse :: String -> (Set (Int,Int),(Int,Int))
parse = toState . concatMap parseRow . zip [0..] . lines
  where
    parseRow (row,line) = [((col,row),ch) | (col,ch) <- zip [0..] line, ch == 'S' || ch == '.']
    toState cells = (fromList (map fst cells),fst $ head $ filter ((== 'S') . snd) cells)

walk :: Int -> (Set (Int,Int),(Int,Int)) -> Set (Int,Int)
walk nsteps (garden,startXY) = go empty [(startXY,0)]
  where
    go :: Set (Int,Int) -> [((Int,Int),Int)] -> Set (Int,Int)
    go set [] = set
    go set ((xy,n):queue)
      | n > nsteps || member xy set = go set queue
      | otherwise = go (insert xy set) (queue ++ concat [continue xy dxy | dxy <- [(1,0),(-1,0),(0,1),(0,-1)]])
      where
        continue :: (Int,Int) -> (Int,Int) -> [((Int,Int),Int)]
        continue (x,y) (dx,dy)
          | not (member (x+dx,y+dy) garden) = []
          | n >= nsteps = []
          | member (x+dx,y+dy) set = []
          | otherwise = [((x+dx,y+dy),n+1)]

reachable :: Int -> (Int,Int) -> (Int,Int) -> Bool
reachable nsteps (startX,startY) (x,y) = odd nsteps == odd (abs (x-startX) + abs (y-startY))

result :: Int -> String -> Int
result nsteps input = length $ filter (reachable nsteps startXY) $ toList $ walk nsteps (garden,startXY)
  where
    (garden,startXY) = parse input

testData :: String
testData = unlines [
    "...........",
    ".....###.#.",
    ".###.##..#.",
    "..#.#...#..",
    "....#.#....",
    ".##..S####.",
    ".##..#...#.",
    ".......##..",
    ".##.#.####.",
    ".##..##.##.",
    "..........."
    ]

test :: ()
test
  | result 6 testData /= 16 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (result 64) $ readFile "input/21.txt"

-- Note that in both the test data and my input, the edges where the maps
-- joins are clear of rocks.
--
-- Note that in my input, but not in the test data, the start row and column
-- are clear of rocks.
-- That makes the test data more difficult, since the there are two ways to
-- get to partially reachable tiles (i.e. when going N, the tile may
-- be entered from the SW or SE corner and distance from start to the
-- corners are different.
--
-- The shortest path to every tile NW of the start tile is to its SE corner.
-- The shortest path to every tile NE of the start tile is to its SW corner.
-- The shortest path to every tile SW of the start tile is to its NE corner.
-- The shortest path to every tile SE of the start tile is to its NW corner.

-- The shortest path to every tile N of the start tile in my input is to the
-- start column on the S edge.
-- The shortest path to every tile E of the start tile in my input is to the
-- start row on the W edge.
-- The shortest path to every tile S of the start tile in my input is to the
-- start column on the N edge.
-- The shortest path to every tile W of the start tile in my input is to the
-- start row on the E edge.

-- The tile in my input is 131×131, and the length and width are both odd, so
-- every other tile has a different set of reachable spaces.

-- The tile in the test input is 11×11.

-- This should makes a diamond shape of tiles to evaluate, where every
-- interior tile has every reachable space within range, and the ones on
-- the edges need to be evaluated for spaces within range.
-- All the ones marked 1 that are NE of the start tile have the same number.
-- All the ones marked 1 that are NW of the start tile have the same number.
-- Similarly for SE, SW.  Also, the one N, the one S, and the one E, and the
-- one W, needs to be evaluated.
-- Similarly for the ones marked 2, and 3, however thick the edge of the
-- diamond needs to be.
-- 
--                    1
--                   121
--                  12321
--                 123.321
--                123...321
--               123.....321
--                123...321
--                 123.321
--                  12321
--                   121
--                    1

walkFrom :: Set (Int,Int) -> [((Int,Int),Int)] -> Map (Int,Int) Int
walkFrom garden startXYs = go Data.Map.empty startXYs
  where
    go dists [] = dists
    go dists ((xy,dist):queue)
      | Data.Map.member xy dists = go dists queue
      | otherwise = go (Data.Map.insert xy dist dists) (queue ++ concat [continue xy dxy | dxy <- [(1,0),(-1,0),(0,1),(0,-1)]])
      where
        continue (x,y) (dx,dy)
          | not (member (x+dx,y+dy) garden) = []
          | Data.Map.member (x+dx,y+dy) dists = []
          | otherwise = [((x+dx,y+dy),dist+1)]

countReachable :: Int -> Int -> Map (Int,Int) Int -> Int
countReachable nsteps stepsToEdge distsFromEdge =
    length $ filter isReachable $ Data.Map.elems distsFromEdge
  where
    isReachable distFromEdge = stepsToEdge + distFromEdge <= nsteps && odd (stepsToEdge + distFromEdge) == odd nsteps

countDir :: Set (Int,Int) -> (Int,Int) -> Int -> [((Int,Int),(Int,Int))] -> Bool -> Int -> Int
countDir garden startXY tileSize toNextTileXYs linear nsteps =
    traceShow (toNextTileXYs,linear,nsteps,fullyReachableM,reachableM)
    fullyReachableN + partiallyReachableN
  where
    tileDists = walkFrom garden [(nextTileStartXY,((walkFrom garden [(startXY,0)])!exitStartTileXY) + (if linear then 1 else 2)) | (exitStartTileXY,nextTileStartXY) <- toNextTileXYs]

    fullyReachableM :: Int
    fullyReachableM = max 0 $ (nsteps - maximum tileDists) `div` tileSize
    reachableM = max 0 $ (nsteps - minimum tileDists) `div` tileSize

    fullyReachableN :: Int
    fullyReachableN
      | linear = (countReachable nsteps 0 tileDists)*((fullyReachableM+1) `div` 2) + (countReachable nsteps tileSize tileDists)*(fullyReachableM `div` 2)
      | otherwise = (countReachable nsteps 0 tileDists)*(fullyReachableM `div` 2)^2 + (countReachable nsteps tileSize tileDists)*(let i = fullyReachableM`div` 2 in (2*i+2)*i`div`2)

    partiallyReachableN
      | linear = sum [countReachable nsteps (m*tileSize) tileDists | m <- [fullyReachableM .. reachableM]]
      | otherwise = sum [countReachable nsteps (m*tileSize) tileDists*m | m <- [fullyReachableM .. reachableM]]

result2 :: String -> Int
result2 input = 
    countDir garden startXY 131 [((65,0),    (65,130))]  True  nsteps + -- N
    countDir garden startXY 131 [((0,0),     (130,130))] False nsteps + -- NW
    countDir garden startXY 131 [((0,65),    (130,65))]  True  nsteps + -- W
    countDir garden startXY 131 [((0,130),   (130,0))]   False nsteps + -- SW
    countDir garden startXY 131 [((65,130),  (65,0))]    True  nsteps + -- S
    countDir garden startXY 131 [((130,130), (0,0))]     False nsteps + -- SE
    countDir garden startXY 131 [((130,65),  (0,65))]    True  nsteps + -- E
    countDir garden startXY 131 [((130,0),   (0,130))]   False nsteps + -- NE
    countReachable nsteps 0 (walkFrom garden [(startXY,0)])
  where
    nsteps = 26501365
    (garden,startXY) = parse input

-- 592712209696441 is too low

testResult2 :: Int -> [Int]
testResult2 nsteps = 
    [countDir garden startXY 11 [((0,0),(0,10)),((10,0),(10,10))] True nsteps, -- N
    countDir garden startXY 11 [((0,0),(10,10))] False nsteps, -- NW
    countDir garden startXY 11 [((0,0),(10,0)),((0,10),(10,10))] True  nsteps, -- W
    countDir garden startXY 11 [((0,10),(10,0))] False nsteps, -- SW
    countDir garden startXY 11 [((0,10),(0,0)),((10,10),(0,10))] True  nsteps, -- S
    countDir garden startXY 11 [((10,10),(0,0))] False nsteps, -- SE
    countDir garden startXY 11 [((10,0),(0,0)),((10,10),(0,10))] True  nsteps, -- E
    countDir garden startXY 11 [((10,0),(0,10))] False nsteps, -- NE
    countReachable nsteps 0 (walkFrom garden [(startXY,0)]),
    0]
  where
    (garden,startXY) = parse testData

testData2 :: String
testData2 = unlines [
    ".................................",
    ".....###.#......###.#......###.#.",
    ".###.##..#..###.##..#..###.##..#.",
    "..#.#...#....#.#...#....#.#...#..",
    "....#.#........#.#........#.#....",
    ".##...####..##...####..##...####.",
    ".##..#...#..##..#...#..##..#...#.",
    ".......##.........##.........##..",
    ".##.#.####..##.#.####..##.#.####.",
    ".##..##.##..##..##.##..##..##.##.",
    ".................................",
    ".................................",
    ".....###.#......###.#......###.#.",
    ".###.##..#..###.##..#..###.##..#.",
    "..#.#...#....#.#...#....#.#...#..",
    "....#.#........#.#........#.#....",
    ".##...####..##..S####..##...####.",
    ".##..#...#..##..#...#..##..#...#.",
    ".......##.........##.........##..",
    ".##.#.####..##.#.####..##.#.####.",
    ".##..##.##..##..##.##..##..##.##.",
    ".................................",
    ".................................",
    ".....###.#......###.#......###.#.",
    ".###.##..#..###.##..#..###.##..#.",
    "..#.#...#....#.#...#....#.#...#..",
    "....#.#........#.#........#.#....",
    ".##...####..##...####..##...####.",
    ".##..#...#..##..#...#..##..#...#.",
    ".......##.........##.........##..",
    ".##.#.####..##.#.####..##.#.####.",
    ".##..##.##..##..##.##..##..##.##.",
    "................................."
    ]

test2 :: ()
test2
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/21.txt"

-- 592712209696441 is too low
