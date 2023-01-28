{-
--- Day 24: Lobby Layout ---

Your raft makes it to the tropical island; it turns out that the small crab was
an excellent navigator. You make your way to the resort.

As you enter the lobby, you discover a small problem: the floor is being
renovated. You can't even reach the check-in desk until they've finished
installing the new tile floor.

The tiles are all hexagonal; they need to be arranged in a hex grid with a very
specific color pattern. Not in the mood to wait, you offer to help figure out
the pattern.

The tiles are all white on one side and black on the other. They start with the
white side facing up. The lobby is large enough to fit whatever pattern might
need to appear there.

A member of the renovation crew gives you a list of the tiles that need to be
flipped over (your puzzle input). Each line in the list identifies a single
tile that needs to be flipped by giving a series of steps starting from a
reference tile in the very center of the room. (Every line starts from the same
reference tile.)

Because the tiles are hexagonal, every tile has six neighbors: east, southeast,
southwest, west, northwest, and northeast. These directions are given in your
list, respectively, as e, se, sw, w, nw, and ne. A tile is identified by a
series of these directions with no delimiters; for example, esenee identifies
the tile you land on if you start at the reference tile and then move one tile
east, one tile southeast, one tile northeast, and one tile east.

Each time a tile is identified, it flips from white to black or from black to
white. Tiles might be flipped more than once. For example, a line like esew
flips a tile immediately adjacent to the reference tile, and a line like
nwwswee flips the reference tile itself.

Here is a larger example:

| sesenwnenenewseeswwswswwnenewsewsw
| neeenesenwnwwswnenewnwwsewnenwseswesw
| seswneswswsenwwnwse
| nwnwneseeswswnenewneswwnewseswneseene
| swweswneswnenwsewnwneneseenw
| eesenwseswswnenwswnwnwsewwnwsene
| sewnenenenesenwsewnenwwwse
| wenwwweseeeweswwwnwwe
| wsweesenenewnwwnwsenewsenwwsesesenwne
| neeswseenwwswnwswswnw
| nenwswwsewswnenenewsenwsenwnesesenew
| enewnwewneswsewnwswenweswnenwsenwsw
| sweneswneswneneenwnewenewwneswswnese
| swwesenesewenwneswnwwneseswwne
| enesenwswwswneneswsenwnewswseenwsese
| wnwnesenesenenwwnenwsewesewsesesew
| nenewswnwewswnenesenwnesewesw
| eneswnwswnwsenenwnwnwwseeswneewsenese
| neswnwewnwnwseenwseesewsenwsweewe
| wseweeenwnesenwwwswnew

In the above example, 10 tiles are flipped once (to black), and 5 more are
flipped twice (to black, then back to white). After all of these instructions
have been followed, a total of 10 tiles are black.

Go through the renovation crew's list and determine which tiles they need to
flip. After all of the instructions have been followed, how many tiles are left
with the black side up?

--- Part Two ---

The tile floor in the lobby is meant to be a living art exhibit. Every day, the
tiles are all flipped according to the following rules:

 - Any black tile with zero or more than 2 black tiles immediately adjacent to
   it is flipped to white.
 - Any white tile with exactly 2 black tiles immediately adjacent to it is
   flipped to black.

Here, tiles immediately adjacent means the six tiles directly touching the tile
in question.

The rules are applied simultaneously to every tile; put another way, it is
first determined which tiles need to be flipped, then they are all flipped at
the same time.

In the above example, the number of black tiles that are facing up after the
given number of days has passed is as follows:

| Day 1: 15
| Day 2: 12
| Day 3: 25
| Day 4: 14
| Day 5: 23
| Day 6: 28
| Day 7: 41
| Day 8: 37
| Day 9: 49
| Day 10: 37
| 
| Day 20: 132
| Day 30: 259
| Day 40: 406
| Day 50: 566
| Day 60: 788
| Day 70: 1106
| Day 80: 1373
| Day 90: 1844
| Day 100: 2208

After executing this process a total of 100 times, there would be 2208 black
tiles facing up.

How many tiles will be black after 100 days?
-}

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
