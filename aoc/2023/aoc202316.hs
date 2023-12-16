import Data.Map(Map,alter,empty,foldrWithKey,fromList,insert,member,size,toList,(!))
import Data.Set(Set)
import qualified Data.Set

parse :: String -> Map (Int,Int) Char
parse = fromList . concatMap parseRow . zip [0..] . lines
  where parseRow (y,line) = zip (map (flip (,) y) [0..]) line

data Dir = N | S | E | W deriving (Eq,Ord,Show)

move :: (Int,Int) -> Dir -> (Int,Int)
move (x,y) N = (x,y-1)
move (x,y) S = (x,y+1)
move (x,y) E = (x+1,y)
move (x,y) W = (x-1,y)

reflect :: Dir -> Char -> Dir
reflect N '/' = E
reflect S '/' = W
reflect E '/' = N
reflect W '/' = S
reflect N '\\' = W
reflect S '\\' = E
reflect E '\\' = S
reflect W '\\' = N

follow :: (Int,Int) -> Dir -> Map (Int,Int) Char -> Map (Int,Int) [Dir]
follow initXY initDir grid = f empty initXY initDir
  where
    f :: Map (Int,Int) [Dir] -> (Int,Int) -> Dir -> Map (Int,Int) [Dir]
    f paths xy dir 
      | not (member xy grid) = paths
      | member xy paths && elem dir (paths!xy) = paths
      | ch == '.' || (ch == '-' && elem dir [E,W]) || (ch == '|' && elem dir [N,S]) = f newPaths (move xy dir) dir
      | ch == '/' || ch == '\\' = f newPaths (move xy reflectDir) reflectDir
      | ch == '|' = f (f newPaths (move xy N) N) (move xy S) S
      | ch == '-' = f (f newPaths (move xy E) E) (move xy W) W
      where
        ch = grid!xy
        newPaths = alter (Just . maybe [dir] (dir:)) xy paths
        reflectDir = reflect dir ch

result :: String -> Int
result = size . follow (0,0) E . parse

testData :: String
testData = unlines [
    ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|...."
    ]

test :: ()
test
  | result testData /= 46 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/16.txt"

result2 :: String -> Int
result2 input = maximum ([size $ follow (0,y) E grid | y <- [0..ymax]]
                      ++ [size $ follow (xmax,y) W grid| y <- [0..ymax]]
                      ++ [size $ follow (x,0) S grid | x <- [0..xmax]]
                      ++ [size $ follow (x,ymax) N grid | x <- [0..xmax]])
  where
    grid = parse input
    xmax = maximum $ map (fst . fst) $ toList grid
    ymax = maximum $ map (snd . fst) $ toList grid

test2 :: ()
test2
  | result2 testData /= 51 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/16.txt"

-- The part 2 code is a bit slow.
-- Try to make it faster by making a table of precalculated paths from each
-- splitter.
-- Goes from about 13 seconds to about 3.2 seconds.  Faster, but still
-- not great.

result1f :: String -> Int
result1f input = Data.Set.size $ Data.Set.map fst $ followf (0,0) E Data.Set.empty grid table
  where
    grid = parse input
    table = makeTable grid

result2f :: String -> Int
result2f input = maximum ([count $ followf (0,y) E Data.Set.empty grid table | y <- [0..ymax]]
                       ++ [count $ followf (xmax,y) W Data.Set.empty grid table | y <- [0..ymax]]
                       ++ [count $ followf (x,0) S Data.Set.empty grid table | x <- [0..xmax]]
                       ++ [count $ followf (x,ymax) N Data.Set.empty grid table | x <- [0..xmax]])
  where
    grid = parse input
    xmax = maximum $ map (fst . fst) $ toList grid
    ymax = maximum $ map (snd . fst) $ toList grid
    count = Data.Set.size . Data.Set.map fst
    table = makeTable grid

makeTable :: Map (Int,Int) Char -> Map (Int,Int) (Set ((Int,Int),Dir))
makeTable grid = foldrWithKey followSplitter empty grid
  where
    followSplitter xy ch table
      | ch == '-' = insert xy (followf xy N (Data.Set.singleton (xy,S)) grid table) table
      | ch == '|' = insert xy (followf xy E (Data.Set.singleton (xy,W)) grid table) table
      | otherwise = table

followf :: (Int,Int) -> Dir -> Set ((Int,Int),Dir) -> Map (Int,Int) Char -> Map (Int,Int) (Set ((Int,Int),Dir)) -> Set ((Int,Int),Dir)
followf xy dir paths grid table
  | Data.Set.member (xy,dir) paths = paths
  | not (member xy grid) = paths
  | ch == '.' || (ch == '-' && elem dir [E,W]) || (ch == '|' && elem dir [N,S]) = followf (move xy dir) dir newPaths grid table
  | ch == '/' || ch == '\\' = followf (move xy reflectDir) reflectDir newPaths grid table
  | member xy table = Data.Set.union paths (table!xy)
  | ch == '|' = followf (move xy N) N (followf (move xy S) S newPaths grid table) grid table
  | ch == '-' = followf (move xy E) E (followf (move xy W) W newPaths grid table) grid table
  where
    ch = grid!xy
    newPaths = Data.Set.insert (xy,dir) paths
    reflectDir = reflect dir ch

test2f :: ()
test2f
  | result1f testData /= 46 = error "a"
  | result2f testData /= 51 = error "a"
  | otherwise = ()

part2f :: IO Int
part2f = fmap result2f $ readFile "input/16.txt"
