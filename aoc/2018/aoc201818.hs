import Data.Map(Map,elems,findWithDefault,fromList,mapKeys,mapWithKey,toList)
import Data.Tuple(swap)

parse :: String -> Map (Int,Int) Char
parse = fromList . p 0 0
  where
    p x y "" = []
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y (c:rest) = ((x,y),c):p (x+1) y rest

is :: Map (Int,Int) Char -> Char -> (Int,Int) -> Bool
is lca char xy = findWithDefault ' ' xy lca == char

adjacent :: (Int,Int) -> [(Int,Int)]
adjacent (x,y) =
    [(x+dx,y+dy) | dx <- [-1,0,1], dy <- [-1,0,1], (dx,dy) /= (0,0)]

resources :: Map (Int,Int) Char -> (Int,Int)
resources lca =
    (length $ filter (== '|') $ elems lca,length $ filter (== '#') $ elems lca)

step :: Map (Int,Int) Char -> Map (Int,Int) Char
step lca = mapWithKey updateAcre lca
  where
    updateAcre xy '|'
      | length (filter (is lca '#') $ adjacent xy) >= 3 = '#'
      | otherwise = '|'
    updateAcre xy '#'
      | length (filter (is lca '#') $ adjacent xy) >= 1
            && length (filter (is lca '|') $ adjacent xy) >= 1 = '#'
      | otherwise = '.'
    updateAcre xy _
      | length (filter (is lca '|') $ adjacent xy) >= 3 = '|'
      | otherwise = '.'

showLCA :: Map (Int,Int) Char -> String
showLCA lca = tail $ concatMap showAcre $ toList $ mapKeys swap lca
  where
    showAcre ((y,0),c) = ['\n',c]
    showAcre (_,c) = [c]

testData :: String
testData = ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."

test :: ()
test
  | resources (head $ drop 10 $ iterate step $ parse testData) /= (37,31) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . resources . head . drop 10 . iterate step . parse) $ readFile "input/18.txt"

-- My input data eventually settles into a 28 step cycle after 467 steps.

part2 :: IO Int
part2 = fmap (uncurry (*) . resources . head . drop (476 + (1000000000 `mod` 28)) . iterate step . parse) $ readFile "input/18.txt"
