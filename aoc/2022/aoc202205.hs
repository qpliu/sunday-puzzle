import Data.Map(Map,adjust,alter,elems,empty,(!))

parse :: String -> (Map Int [Char],[(Int,Int,Int)])
parse = p . span (/= "") . lines
  where
    p (drawing,procedure) = (foldr (parseDrawing 1) empty $ init drawing,map (parseProcedure . words) $ tail procedure)
    parseDrawing stack ('[':crate:']':rest) stacks =
       parseDrawing (stack+1) (drop 1 rest) (alter (Just . maybe [crate] (crate:)) stack stacks)
    parseDrawing stack (' ':' ':' ':rest) stacks =
       parseDrawing (stack+1) (drop 1 rest) stacks
    parseDrawing _ _ stacks = stacks
    parseProcedure ["move",count,"from",a,"to",b] = (read count,read a,read b)

rearrange :: Map Int [Char] -> (Int,Int,Int) -> Map Int [Char]
rearrange stacks (n,from,to)
  | n <= 0 = stacks
  | otherwise = rearrange newStacks (n-1,from,to)
  where
    crate = head $ stacks!from
    newStacks = adjust tail from $ adjust (crate:) to stacks

testData :: String
testData = unlines [
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
    ]

test :: ()
test
  | (map head . elems . uncurry (foldl rearrange) . parse) testData /= "CMZ" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = do
  fmap (map head . elems . uncurry (foldl rearrange) . parse) $ readFile "input/05.txt"

rearrange2 :: Map Int [Char] -> (Int,Int,Int) -> Map Int [Char]
rearrange2 stacks (n,from,to) = adjust (drop n) from $ adjust (crates ++) to stacks
  where
    crates = take n $ stacks!from

test2 :: ()
test2
  | (map head . elems . uncurry (foldl rearrange2) . parse) testData /= "MCD" = error "a"
  | otherwise = ()

part2 :: IO String
part2 = do
  fmap (map head . elems . uncurry (foldl rearrange2) . parse) $ readFile "input/05.txt"
