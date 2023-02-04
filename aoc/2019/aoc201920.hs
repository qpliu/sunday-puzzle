-- Yet another breadth-first search.
import Data.Map(Map,alter,empty,findWithDefault,foldWithKey,(!))
import qualified Data.Map
import Data.Set(Set,difference,elems,intersection,member,fromList,size,union)

parse :: String -> (Set (Int,Int),Map String [(Int,Int)])
parse input = (passages,foldWithKey addPortal empty portalSigns)
  where
    (passages,portalSigns) = p 0 0 [] [] input
    p x y passages portalSigns [] = (fromList passages,Data.Map.fromList portalSigns)
    p x y passages portalSigns (c:cs)
      | c == '\n' = p 0 (y+1) passages portalSigns cs
      | c == ' ' || c == '#' = p (x+1) y passages portalSigns cs
      | c == '.' = p (x+1) y ((x,y):passages) portalSigns cs
      | otherwise = p (x+1) y passages (((x,y),c):portalSigns) cs
    addPortal (x,y) ch portals
      | member (x-1,y) passages && Data.Map.member (x+1,y) portalSigns =
          alter (Just . maybe [(x-1,y)] ((x-1,y):)) [ch,portalSigns!(x+1,y)] portals
      | member (x+1,y) passages && Data.Map.member (x-1,y) portalSigns =
          alter (Just . maybe [(x+1,y)] ((x+1,y):)) [portalSigns!(x-1,y),ch] portals
      | member (x,y-1) passages && Data.Map.member (x,y+1) portalSigns =
          alter (Just . maybe [(x,y-1)] ((x,y-1):)) [ch,portalSigns!(x,y+1)] portals
      | member (x,y+1) passages && Data.Map.member (x,y-1) portalSigns =
          alter (Just . maybe [(x,y+1)] ((x,y+1):)) [portalSigns!(x,y-1),ch] portals
      | otherwise = portals

makeConnections :: Map String [(Int,Int)] -> Map (Int,Int) (Int,Int)
makeConnections portals = Data.Map.fromList $ concatMap makeConn $ Data.Map.elems portals
  where
    makeConn [a,b] = [(a,b),(b,a)]
    makeConn _ = []

nexts :: (Set (Int,Int),Map (Int,Int) (Int,Int)) -> (Int,Int) -> [(Int,Int)]
nexts (passages,connections) xy@(x,y) = filter (`elem` passages) [(x+1,y),(x-1,y),(x,y+1),(x,y-1),findWithDefault (-1,-1) xy connections]

search :: (Set (Int,Int),Map (Int,Int) (Int,Int)) -> Set (Int,Int) -> Int -> Set (Int,Int) -> Set (Int,Int) -> Int
search maze goal nsteps seen current
  | size (intersection goal current) > 0 = nsteps
  | otherwise = search maze goal (nsteps+1) (union seen next) (difference next seen)
  where next = fromList (concatMap (nexts maze) $ elems current)

run :: String -> Int
run input = search (passages,connections) goal 0 start start
  where
    (passages,portals) = parse input
    connections = makeConnections portals
    goal = fromList $ portals!"ZZ"
    start = fromList $ portals!"AA"

testData :: [(Int,String)]
testData = [
    (23,"         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "),
    (58,"                   A               \n                   A               \n  #################.#############  \n  #.#...#...................#.#.#  \n  #.#.#.###.###.###.#########.#.#  \n  #.#.#.......#...#.....#.#.#...#  \n  #.#########.###.#####.#.#.###.#  \n  #.............#.#.....#.......#  \n  ###.###########.###.#####.#.#.#  \n  #.....#        A   C    #.#.#.#  \n  #######        S   P    #####.#  \n  #.#...#                 #......VT\n  #.#.#.#                 #.#####  \n  #...#.#               YN....#.#  \n  #.###.#                 #####.#  \nDI....#.#                 #.....#  \n  #####.#                 #.###.#  \nZZ......#               QG....#..AS\n  ###.###                 #######  \nJO..#.#.#                 #.....#  \n  #.#.#.#                 ###.#.#  \n  #...#..DI             BU....#..LF\n  #####.#                 #.#####  \nYN......#               VT..#....QG\n  #.###.#                 #.###.#  \n  #.#...#                 #.....#  \n  ###.###    J L     J    #.#.###  \n  #.....#    O F     P    #.#...#  \n  #.###.#####.#.#####.#####.###.#  \n  #...#.#.#...#.....#.....#.#...#  \n  #.#####.###.###.#.#.#########.#  \n  #...#.#.....#...#.#.#.#.....#.#  \n  #.###.#####.###.###.#.#.#######  \n  #.#.........#...#.............#  \n  #########.###.###.#############  \n           B   J   C               \n           U   P   P               ")
    ]

test :: ()
test
  | any (uncurry (/=) . fmap run) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/20.txt"

nexts2 :: ((Int,Int,Int,Int),Set (Int,Int),Map (Int,Int) (Int,Int)) -> ((Int,Int),Int) -> [((Int,Int),Int)]
nexts2 ((xmin,ymin,xmax,ymax),passages,connections) ((x,y),level) = filter valid [((x+1,y),level),((x-1,y),level),((x,y+1),level),((x,y-1),level),(findWithDefault (-1,-1) (x,y) connections,nextLevel)]
  where
    valid (xy,level) = xy `elem` passages && level >= 0
    nextLevel | x == xmin || x == xmax || y == ymin || y == ymax = level - 1
              | otherwise = level + 1

search2 :: ((Int,Int,Int,Int),Set (Int,Int),Map (Int,Int) (Int,Int)) -> Set ((Int,Int),Int) -> Int -> Set ((Int,Int),Int) -> Set ((Int,Int),Int) -> Int
search2 maze goal nsteps seen current
  | size (intersection goal current) > 0 = nsteps
  | otherwise = search2 maze goal (nsteps+1) (union seen next) (difference next seen)
  where next = fromList (concatMap (nexts2 maze) $ elems current)

run2 :: String -> Int
run2 input = search2 (xyrange,passages,connections) goal 0 start start
  where
    (passages,portals) = parse input
    connections = makeConnections portals
    goal = fromList $ map (flip (,) 0) $ portals!"ZZ"
    start = fromList $ map (flip (,) 0) $ portals!"AA"
    xyrange = (minimum $ map fst $ elems passages,minimum $ map snd $ elems passages,maximum $ map fst $ elems passages,maximum $ map snd $ elems passages)

testData2 :: String
testData2 = "             Z L X W       C                 \n             Z P Q B       K                 \n  ###########.#.#.#.#######.###############  \n  #...#.......#.#.......#.#.......#.#.#...#  \n  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n  #.###.#######.###.###.#.###.###.#.#######  \n  #...#.......#.#...#...#.............#...#  \n  #.#########.#######.#.#######.#######.###  \n  #...#.#    F       R I       Z    #.#.#.#  \n  #.###.#    D       E C       H    #.#.#.#  \n  #.#...#                           #...#.#  \n  #.###.#                           #.###.#  \n  #.#....OA                       WB..#.#..ZH\n  #.###.#                           #.#.#.#  \nCJ......#                           #.....#  \n  #######                           #######  \n  #.#....CK                         #......IC\n  #.###.#                           #.###.#  \n  #.....#                           #...#.#  \n  ###.###                           #.#.#.#  \nXF....#.#                         RF..#.#.#  \n  #####.#                           #######  \n  #......CJ                       NM..#...#  \n  ###.#.#                           #.###.#  \nRE....#.#                           #......RF\n  ###.###        X   X       L      #.#.#.#  \n  #.....#        F   Q       P      #.#.#.#  \n  ###.###########.###.#######.#########.###  \n  #.....#...#.....#.......#...#.....#.#...#  \n  #####.#.###.#######.#######.###.###.#.#.#  \n  #.......#.......#.#.#.#.#...#...#...#.#.#  \n  #####.###.#####.#.#.#.#.###.###.#.###.###  \n  #.......#.....#.#...#...............#...#  \n  #############.#.#.###.###################  \n               A O F   N                     \n               A A D   M                     "
test2 :: ()
test2
  | run2 testData2 /= 396 = error "a"
  | otherwise = ()

-- This is really slow.
-- Calculating a graph of all shortest paths between all portals for all
-- levels below the outer level would be faster.
part2 :: IO Int
part2 = fmap run2 $ readFile "input/20.txt"
