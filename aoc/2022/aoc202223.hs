import Data.Set(Set,fold,fromList,member,size)
import qualified Data.Set
import Data.Map(Map,alter,empty,insert,toList)

parse :: String -> Set (Int,Int)
parse = fromList . p 0 0
  where
    p _ _ [] = []
    p x y (c:rest)
      | c == '\n' = p 0 (y+1) rest
      | c == '#' = (x,y) : p (x+1) y rest
      | otherwise = p (x+1) y rest

proposeMove :: (Set (Int,Int),Int) -> (Map (Int,Int) [(Int,Int)],Int)
proposeMove (elves,roundNum) = (fold prop empty elves,roundNum)
  where
    prop xy@(x,y) props
      | not $ or [n,ne,nw,e,w,s,se,sw] = insert xy [xy] props
      | r < 1 && not (or [n,ne,nw]) = alter add (x,y-1) props
      | r < 2 && not (or [s,se,sw]) = alter add (x,y+1) props
      | r < 3 && not (or [w,nw,sw]) = alter add (x-1,y) props
      |          not (or [e,ne,se]) = alter add (x+1,y) props
      | r > 0 && not (or [n,ne,nw]) = alter add (x,y-1) props
      | r > 1 && not (or [s,se,sw]) = alter add (x,y+1) props
      | r > 2 && not (or [w,nw,sw]) = alter add (x-1,y) props
      | otherwise = insert xy [xy] props
      where
        add = Just . maybe [xy] (xy:)
        r = roundNum `mod` 4
        n = member (x,y-1) elves
        ne = member (x+1,y-1) elves
        nw = member (x-1,y-1) elves
        e = member (x+1,y) elves
        w = member (x-1,y) elves
        s = member (x,y+1) elves
        se = member (x+1,y+1) elves
        sw = member (x-1,y+1) elves

executeMove :: (Map (Int,Int) [(Int,Int)],Int) -> (Set (Int,Int),Int)
executeMove (props,roundNum) = (fromList $ concatMap exec $ toList props,roundNum+1)
  where
    exec (xy,srcs)
      | length srcs == 1 = [xy]
      | otherwise = srcs

groundCount :: Set (Int,Int) -> Int
groundCount elves = (xmax-xmin+1)*(ymax-ymin+1) - size elves
  where
    xmax = maximum $ Data.Set.map fst elves
    xmin = minimum $ Data.Set.map fst elves
    ymax = maximum $ Data.Set.map snd elves
    ymin = minimum $ Data.Set.map snd elves

display :: Set (Int,Int) -> String
display elves = unlines [[if member (x,y) elves then '#' else '.' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    xmax = maximum $ Data.Set.map fst elves
    xmin = minimum $ Data.Set.map fst elves
    ymax = maximum $ Data.Set.map snd elves
    ymin = minimum $ Data.Set.map snd elves

testData :: [String]
testData = [unlines [
    ".....",
    "..##.",
    "..#..",
    ".....",
    "..##.",
    "....."
    ], unlines [
    "....#..",
    "..###.#",
    "#...#.#",
    ".#...##",
    "#.###..",
    "##.#.##",
    ".#..#.."
    ]]

test :: ()
test
  | (groundCount . fst . head . drop 10 . iterate (executeMove . proposeMove) . flip (,) 0 . parse) (testData !! 1) /= 110 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (groundCount . fst . head . drop 10 . iterate (executeMove . proposeMove) . flip (,) 0 . parse) $ readFile "input/23.txt"

findFixed :: (Set (Int,Int),Int) -> Int
findFixed elves@(set,_)
  | newSet == set = roundNum
  | otherwise = findFixed newElves
  where newElves@(newSet,roundNum) = executeMove $ proposeMove elves

test2 :: ()
test2
  | (findFixed . flip (,) 0 . parse) (testData !! 0) /= 4 = error "a"
  | (findFixed . flip (,) 0 . parse) (testData !! 1) /= 20 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (findFixed . flip (,) 0 . parse) $ readFile "input/23.txt"
