import Data.Map(Map,(!))
import qualified Data.Map
import Data.Set(Set,empty,fromList,insert,member,toList)
import Data.Tuple(swap)

parse :: String -> (Set (Int,Int),Set (Int,Int),Int,Int)
parse = gather [] [] 0 0 . zip [0..] . lines
  where
    gather rounds cubes row col [] = (fromList rounds,fromList cubes,row+1,col+1)
    gather rounds cubes _ _ ((row,line):rest) =
      gatherRow rounds cubes row 0 rest (zip [0..] line)
    gatherRow rounds cubes row col rest [] = gather rounds cubes row col rest
    gatherRow rounds cubes row _ rest ((col,ch):rowRest)
      | ch == 'O' = gatherRow ((col,row):rounds) cubes row col rest rowRest
      | ch == '#' = gatherRow rounds ((col,row):cubes) row col rest rowRest
      | otherwise = gatherRow rounds cubes row col rest rowRest

slideNorth :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
slideNorth cubes rounds = foldl slideRock empty $ toList rounds
    -- this depends on the ascending ordering of the elements in toList rounds
  where
    slideRock slid (col,row)
      | row <= 0 || (col,row-1) `member` slid || (col,row-1) `member` cubes =
          insert (col,row) slid
      | otherwise = slideRock slid (col,row-1)

load :: Int -> Set (Int,Int) -> Int
load southEdge rounds = sum $ map ((southEdge -) . snd) $ toList rounds

result :: String -> Int
result input = load southEdge $ slideNorth cubes rounds
  where (rounds,cubes,southEdge,eastEdge) = parse input

testData :: String
testData = unlines [
    "O....#....",
    "O.OO#....#",
    ".....##...",
    "OO.#O....O",
    ".O.....O#.",
    "O.#..O.#.#",
    "..O..#O..O",
    ".......O..",
    "#....###..",
    "#OO..#...."
    ]

test :: ()
test
  | result testData /= 136 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/14.txt"

slideWest :: Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
slideWest cubes rounds = foldl slideRock empty $ toList rounds
    -- this depends on the ascending ordering of the elements in toList rounds
  where
    slideRock slid (col,row)
      | col <= 0 || (col-1,row) `member` slid || (col-1,row) `member` cubes =
          insert (col,row) slid
      | otherwise = slideRock slid (col-1,row)

slideSouth :: Int -> Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
slideSouth southEdge cubes rounds = foldr slideRock empty $ toList rounds
    -- this depends on the ascending ordering of the elements in toList rounds
  where
    slideRock (col,row) slid
      | row+1 >= southEdge || (col,row+1) `member` slid || (col,row+1) `member` cubes =
          insert (col,row) slid
      | otherwise = slideRock (col,row+1) slid

slideEast :: Int -> Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
slideEast eastEdge cubes rounds = foldr slideRock empty $ toList rounds
    -- this depends on the ascending ordering of the elements in toList rounds
  where
    slideRock (col,row) slid
      | col+1 >= eastEdge || (col+1,row) `member` slid || (col+1,row) `member` cubes =
          insert (col,row) slid
      | otherwise = slideRock (col+1,row) slid

spin :: Int -> Int -> Set (Int,Int) -> Set (Int,Int) -> Set (Int,Int)
spin southEdge eastEdge cubes = slideEast eastEdge cubes . slideSouth southEdge cubes . slideWest cubes . slideNorth cubes

findCycle :: Ord a => (a -> a) -> a -> (Int,Int,Map Int a)
findCycle f a0 = search a0 0 Data.Map.empty
  where
    search ai i table
      | Data.Map.member ai table = (table!ai,i,Data.Map.fromList $ map swap $ Data.Map.toList table)
      | otherwise = search (f ai) (i+1) (Data.Map.insert ai i table)

result2 :: String -> Int
result2 input = load southEdge finalRounds
  where
    (rounds,cubes,southEdge,eastEdge) = parse input
    (i2,i1,table) = findCycle (spin southEdge eastEdge cubes) rounds
    finalRounds = table!((1000000000-i1) `mod` (i2-i1) + i1)

test2 :: ()
test2
  | spin southEdge eastEdge cubes rounds /= spin1 = error "a"
  | spin southEdge eastEdge cubes spin1 /= spin2 = error "a"
  | spin southEdge eastEdge cubes spin2 /= spin3 = error "a"
  | result2 testData /= 64 = error "a"
  | otherwise = ()
  where
    (rounds,cubes,southEdge,eastEdge) = parse testData
    [(spin1,_,_,_),(spin2,_,_,_),(spin3,_,_,_)] = map (parse . unlines) [[
        ".....#....",
        "....#...O#",
        "...OO##...",
        ".OO#......",
        ".....OOO#.",
        ".O#...O#.#",
        "....O#....",
        "......OOOO",
        "#...O###..",
        "#..OO#...."
        ],[
        ".....#....",
        "....#...O#",
        ".....##...",
        "..O#......",
        ".....OOO#.",
        ".O#...O#.#",
        "....O#...O",
        ".......OOO",
        "#..OO###..",
        "#.OOO#...O"
        ],[
        ".....#....",
        "....#...O#",
        ".....##...",
        "..O#......",
        ".....OOO#.",
        ".O#...O#.#",
        "....O#...O",
        ".......OOO",
        "#...O###.O",
        "#.OOO#...O"
        ]]

part2 :: IO Int
part2 = fmap result2 $ readFile "input/14.txt"
