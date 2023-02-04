-- Yet another breadth-first search.  It will probably be slow.
import Data.Char(isLower,isUpper,toLower)
import Data.Set(Set,difference,elems,empty,fromList,insert,member,size,union)
import Data.Map(Map,delete,findWithDefault)
import qualified Data.Map

parse :: String -> (Int,(Int,Int),(Int,Int),Map (Int,Int) Char)
parse = p 0 0 (0,0) (0,0) []
  where
    p nkeys xmax startxy (x,y) mapList [] = (nkeys,(xmax,y),startxy,Data.Map.fromList mapList)
    p nkeys xmax startxy (x,y) mapList (c:cs)
      | c == '\n' = p nkeys xmax startxy (0,y+1) mapList cs
      | c == '.' || isUpper c = p nkeys (max x xmax) startxy (x+1,y) (((x,y),c):mapList) cs
      | c == '@' = p nkeys (max x xmax) (x,y) (x+1,y) (((x,y),'.'):mapList) cs
      | isLower c = p (nkeys+1) (max x xmax) startxy (x+1,y) (((x,y),c):mapList) cs
      | otherwise = p nkeys xmax startxy (x+1,y) mapList cs

type State = ((Int,Int),Set Char)

nextStates :: Map (Int,Int) Char -> State -> [State]
nextStates m ((x,y),keys) = concat [go (x+1,y),go (x-1,y),go (x,y+1),go (x,y-1)]
  where
    go xy = nextState xy (findWithDefault '#' xy m)
    nextState xy c
      | c == '.' = [(xy,keys)]
      | isLower c = [(xy,insert c keys)]
      | isUpper c && toLower c `member` keys = [(xy,keys)]
      | otherwise = []

hasAllKeys :: Int -> State -> Bool
hasAllKeys nkeys (_,keys) = nkeys == size keys

search :: (Int,(Int,Int),(Int,Int),Map (Int,Int) Char) -> Int -> Set State -> Set State -> Int
search p@(nkeys,_,_,m) nsteps seen current
  | any (hasAllKeys nkeys) current = nsteps
  | otherwise = search p (nsteps+1) (union seen next) (difference next seen)
  where next = fromList (concatMap (nextStates m) $ elems current)

run :: String -> Int
run input = search p 0 start start
  where
    p@(_,_,startxy,_) = parse input
    start = fromList [(startxy,empty)]

testData :: [(Int,String)]
testData = [
    (8,"#########\n#b.A.@.a#\n#########"),
    (86,"########################\n#f.D.E.e.C.b.A.@.a.B.c.#\n######################.#\n#d.....................#\n########################"),
    (132,"########################\n#...............b.C.D.f#\n#.######################\n#.....@.a.B.c.d.A.e.F.g#\n########################"),
    (136,"#################\n#i.G..c...e..H.p#\n########.########\n#j.A..b...f..D.o#\n########@########\n#k.E..a...g..B.n#\n########.########\n#l.F..d...h..C.m#\n#################"),
    (81,"########################\n#@..............ac.GI.b#\n###d#e#f################\n###A#B#C################\n###g#h#i################\n########################")
    ]

test :: ()
test
  | any (uncurry (/=) . fmap run) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/18.txt"

-- A straightforward breadth-first search is too slow.

-- Don't move robots in quadrants where all the keys are found.

-- Only move robots in the first quadrant until hitting a locked door with
-- a key in another quadrant.  Then, only move robots in the second quadrant
-- until hitting a locked door with a key in another quadrant.  Etc, and hope
-- there is no deadlock.

-- After looking at hints on the internet, there won't be a deadlock for the
-- test data and not for any known input data.

run2 :: String -> Int
run2 input = sum [search p2 0 start start | start <- starts]
  where
    p@(nkeys,xymax,(x,y),m) = parse input
    p2 = (nkeys,xymax,(x,y),delete (x,y-1) $ delete (x,y+1) $
                            delete (x-1,y) $ delete (x+1,y) $ delete (x,y) m)
    starts = [fromList [((x-1,y-1),getKeys (<x) (<y))],
              fromList [((x+1,y-1),getKeys (>x) (<y))],
              fromList [((x-1,y+1),getKeys (<x) (>y))],
              fromList [((x+1,y+1),getKeys (>x) (>y))]]
    getKeys testx testy = fromList [c | ((x,y),c) <- Data.Map.toList m, isLower c, not (testx x && testy y)]

testData2 :: [(Int,String)]
testData2 = [
    (8,"#######\n#a.#Cd#\n##...##\n##.@.##\n##...##\n#cB#Ab#\n#######"),
    (24,"###############\n#d.ABC.#.....a#\n######...######\n######.@.######\n######...######\n#b.....#.....c#\n###############"),
    (32,"#############\n#DcBa.#.GhKl#\n#.###...#I###\n#e#d#.@.#j#k#\n###C#...###J#\n#fEbA.#.FgHi#\n#############"),
    (72,"#############\n#g#f.D#..h#l#\n#F###e#E###.#\n#dCba...BcIJ#\n#####.@.#####\n#nK.L...G...#\n#M###N#H###.#\n#o#m..#i#jk.#\n#############")
    ]

test2 :: ()
test2
  | any (uncurry (/=) . fmap run2) testData2 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap run2 $ readFile "input/18.txt"
