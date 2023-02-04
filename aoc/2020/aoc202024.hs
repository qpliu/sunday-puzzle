import Data.Map(Map,alter,empty,findWithDefault,toList)
import qualified Data.Map
import Data.Set(elems,fromList)

parse :: String -> Map (Int,Int) Int
parse = foldr flipTile empty . lines
  where
    flipTile path tiles = alter (Just . maybe 1 (1-)) (findTile (0,0) path) tiles

findTile :: (Int,Int) -> String -> (Int,Int)
findTile (x,y) ('e':rest) = findTile (x+1,y) rest
findTile (x,y) ('w':rest) = findTile (x-1,y) rest
findTile (x,y) ('n':'e':rest) = findTile (x+if odd y then 0 else 1,y-1) rest
findTile (x,y) ('n':'w':rest) = findTile (x-if odd y then 1 else 0,y-1) rest
findTile (x,y) ('s':'e':rest) = findTile (x+if odd y then 0 else 1,y+1) rest
findTile (x,y) ('s':'w':rest) = findTile (x-if odd y then 1 else 0,y+1) rest
findTile (x,y) _ = (x,y)

testData :: String
testData = "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew\n"

test :: ()
test
  | (sum . parse) testData /= 10 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . parse) $ readFile "input/24.txt"

neighbors :: (Int,Int) -> [(Int,Int)]
neighbors (x,y)
  | odd y = [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x-1,y-1),(x-1,y+1)]
  | otherwise = [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x+1,y-1),(x+1,y+1)]

step :: Map (Int,Int) Int -> Map (Int,Int) Int
step tiles = Data.Map.fromList $ map update $ concatMap (neighbors . fst) $ filter ((> 0) . snd) $ toList tiles
  where
    update xy
      | n == 0 || n > 2 = (xy,0)
      | n == 1 = (xy,i)
      | n == 2 = (xy,1)
      where
        n = sum $ map (flip (findWithDefault 0) tiles) (neighbors xy)
        i = findWithDefault 0 xy tiles

run2 :: Int -> String -> Int
run2 days input = sum $ head $ drop days $ iterate step $ parse input

test2 :: ()
test2
  | run2 1 testData /= 15 = error "a"
  | run2 2 testData /= 12 = error "a"
  | run2 3 testData /= 25 = error "a"
  | run2 4 testData /= 14 = error "a"
  | run2 5 testData /= 23 = error "a"
  | run2 6 testData /= 28 = error "a"
  | run2 7 testData /= 41 = error "a"
  | run2 8 testData /= 37 = error "a"
  | run2 9 testData /= 49 = error "a"
  | run2 10 testData /= 37 = error "a"
  | run2 20 testData /= 132 = error "a"
  | run2 30 testData /= 259 = error "a"
  | run2 40 testData /= 406 = error "a"
  | run2 50 testData /= 566 = error "a"
  | run2 60 testData /= 788 = error "a"
  | run2 70 testData /= 1106 = error "a"
  | run2 80 testData /= 1373 = error "a"
  | run2 90 testData /= 1844 = error "a"
  | run2 100 testData /= 2208 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (run2 100) $ readFile "input/24.txt"
