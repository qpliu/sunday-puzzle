import Data.List(sort)
import Data.Map(Map,empty,fromList,insert,keys,member,size,(!))
import Data.Maybe(catMaybes)

parse :: String -> Map (Int,Int) Char
parse = fromList . concatMap parseLine . zip [0..] . lines
  where
    parseLine (y,line) = [((x,y),ch) | (x,ch) <- zip [0..] line, ch /= '#']

toGraph :: Map (Int,Int) Char -> Map (Int,Int) [(Int,(Int,Int))]
toGraph trails = makeNodes empty [start,finish]
  where
    start = minimum $ keys trails
    finish = maximum $ keys trails

    makeNodes :: Map (Int,Int) [(Int,(Int,Int))] -> [(Int,Int)] -> Map (Int,Int) [(Int,(Int,Int))]
    makeNodes graph [] = graph
    makeNodes graph (xy:queue)
      | member xy graph = makeNodes graph queue
      | otherwise = makeNodes (insert xy neighbors graph) (queue ++ map snd neighbors)
      where
        neighbors = reverse $ sort $ catMaybes [followTrail xy dxy 1 | dxy <- [(1,0),(-1,0),(0,1),(0,-1)]]

    followTrail :: (Int,Int) -> (Int,Int) -> Int -> Maybe (Int,(Int,Int))
    followTrail xy@(x,y) dxy@(dx,dy) steps
      | newXY == finish = Just (steps,newXY)
      | not (member newXY trails) = Nothing
      | ch == '>' && dxy /= (1,0) = Nothing
      | ch == '<' && dxy /= (-1,0) = Nothing
      | ch == '^' && dxy /= (0,-1) = Nothing
      | ch == 'v' && dxy /= (0,1) = Nothing
      | length newDXY == 0 = Nothing
      | length newDXY == 1 = followTrail newXY (head newDXY) (steps+1)
      | otherwise = Just (steps,newXY)
      where
        newXY = (x+dx,y+dy)
        newDXY = [ndxy | ndxy@(ndx,ndy) <- [(1,0),(-1,0),(0,1),(0,-1)], (ndx+dx,ndy+dy) /= (0,0), member (x+dx+ndx,y+dy+ndy) trails]
        ch = trails!newXY

-- depth first search
-- my input is a 36-node graph
-- part 2 takes about 52 seconds
longestPath :: Map (Int,Int) [(Int,(Int,Int))] -> Int
longestPath graph = search start 0 empty
  where
    start = minimum $ keys graph
    (nfinish,finish) = findFinish [] 0 $ maximum $ keys graph

    search xy steps visited
      | xy == finish = steps + nfinish
      | member xy visited = 0
      | otherwise = maximum (0 : [search newXY (steps+newSteps) newVisited | (newSteps,newXY) <- graph!xy])
      where newVisited = insert xy () visited

    findFinish visited n xy
      | length edges /= 1 = (n,xy)
      | otherwise = findFinish (xy:visited) (n+edgeN) edgeXY
      where
        edges = graph!xy
        ((edgeN,edgeXY):_) = edges

result :: String -> Int
result = longestPath . toGraph . parse

testData :: String
testData = unlines [
    "#.#####################",
    "#.......#########...###",
    "#######.#########.#.###",
    "###.....#.>.>.###.#.###",
    "###v#####.#v#.###.#.###",
    "###.>...#.#.#.....#...#",
    "###v###.#.#.#########.#",
    "###...#.#.#.......#...#",
    "#####.#.#.#######.#.###",
    "#.....#.#.#.......#...#",
    "#.#####.#.#.#########v#",
    "#.#...#...#...###...>.#",
    "#.#.#v#######v###.###v#",
    "#...#.>.#...>.>.#.###.#",
    "#####v#.#.###v#.#.###.#",
    "#.....#...#...#.#.#...#",
    "#.#########.###.#.#.###",
    "#...###...#...#...#.###",
    "###.###.#.###v#####v###",
    "#...#...#.#.>.>.#.>.###",
    "#.###.###.#.###.#.#v###",
    "#.....###...###...#...#",
    "#####################.#"
    ]

test :: ()
test
  | result testData /= 94 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/23.txt"

toGraph2 :: Map (Int,Int) Char -> Map (Int,Int) [(Int,(Int,Int))]
toGraph2 trails = makeNodes empty [start,finish]
  where
    start = minimum $ keys trails
    finish = maximum $ keys trails

    makeNodes :: Map (Int,Int) [(Int,(Int,Int))] -> [(Int,Int)] -> Map (Int,Int) [(Int,(Int,Int))]
    makeNodes graph [] = graph
    makeNodes graph (xy:queue)
      | member xy graph = makeNodes graph queue
      | otherwise = makeNodes (insert xy neighbors graph) (queue ++ map snd neighbors)
      where
        neighbors = catMaybes [followTrail xy dxy 1 | dxy <- [(1,0),(-1,0),(0,1),(0,-1)]]

    followTrail :: (Int,Int) -> (Int,Int) -> Int -> Maybe (Int,(Int,Int))
    followTrail xy@(x,y) dxy@(dx,dy) steps
      | newXY == finish = Just (steps,newXY)
      | not (member newXY trails) = Nothing
      | length newDXY == 0 = Nothing
      | length newDXY == 1 = followTrail newXY (head newDXY) (steps+1)
      | otherwise = Just (steps,newXY)
      where
        newXY = (x+dx,y+dy)
        newDXY = [ndxy | ndxy@(ndx,ndy) <- [(1,0),(-1,0),(0,1),(0,-1)], (ndx+dx,ndy+dy) /= (0,0), member (x+dx+ndx,y+dy+ndy) trails]
        ch = trails!newXY

result2 :: String -> Int
result2 = longestPath . toGraph2 . parse

test2 :: ()
test2
  | result2 testData /= 154 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/23.txt"
