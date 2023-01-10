{-
--- Day 12: Subterranean Sustainability ---

The year 518 is significantly more underground than your history books implied.
Either that, or you've arrived in a vast cavern network under the North Pole.

After exploring a little, you discover a long tunnel that contains a row of
small pots as far as you can see to your left and right. A few of them contain
plants - someone is trying to grow things in these geothermally-heated caves.

The pots are numbered, with 0 in front of you. To the left, the pots are
numbered -1, -2, -3, and so on; to the right, 1, 2, 3.... Your puzzle input
contains a list of pots from 0 to the right and whether they do (#) or do not
(.) currently contain a plant, the initial state. (No other pots currently
contain plants.) For example, an initial state of #..##.... indicates that pots
0, 3, and 4 currently contain plants.

Your puzzle input also contains some notes you find on a nearby table: someone
has been trying to figure out how these plants spread to nearby pots. Based on
the notes, for each generation of plants, a given pot has or does not have a
plant based on whether that pot (and the two pots on either side of it) had a
plant in the last generation. These are written as LLCRR => N, where L are pots
to the left, C is the current pot being considered, R are the pots to the
right, and N is whether the current pot will have a plant in the next
generation. For example:

 - A note like ..#.. => . means that a pot that contains a plant but with no
   plants within two pots of it will not have a plant in it during the next
   generation.
 - A note like ##.## => . means that an empty pot with two plants on each side
   of it will remain empty in the next generation.
 - A note like .##.# => # means that a pot has a plant in a given generation
   if, in the previous generation, there were plants in that pot, the one
   immediately to the left, and the one two pots to the right, but not in the
   ones immediately to the right and two to the left.
 - It's not clear what these plants are for, but you're sure it's important, so
   you'd like to make sure the current configuration of plants is sustainable
   by determining what will happen after 20 generations.

For example, given the following input:

| initial state: #..#.#..##......###...###
| 
| ...## => #
| ..#.. => #
| .#... => #
| .#.#. => #
| .#.## => #
| .##.. => #
| .#### => #
| #.#.# => #
| #.### => #
| ##.#. => #
| ##.## => #
| ###.. => #
| ###.# => #
| ####. => #

For brevity, in this example, only the combinations which do produce a plant
are listed. (Your input includes all possible combinations.) Then, the next 20
generations will look like this:

|                  1         2         3     
|        0         0         0         0     
|  0: ...#..#.#..##......###...###...........
|  1: ...#...#....#.....#..#..#..#...........
|  2: ...##..##...##....#..#..#..##..........
|  3: ..#.#...#..#.#....#..#..#...#..........
|  4: ...#.#..#...#.#...#..#..##..##.........
|  5: ....#...##...#.#..#..#...#...#.........
|  6: ....##.#.#....#...#..##..##..##........
|  7: ...#..###.#...##..#...#...#...#........
|  8: ...#....##.#.#.#..##..##..##..##.......
|  9: ...##..#..#####....#...#...#...#.......
| 10: ..#.#..#...#.##....##..##..##..##......
| 11: ...#...##...#.#...#.#...#...#...#......
| 12: ...##.#.#....#.#...#.#..##..##..##.....
| 13: ..#..###.#....#.#...#....#...#...#.....
| 14: ..#....##.#....#.#..##...##..##..##....
| 15: ..##..#..#.#....#....#..#.#...#...#....
| 16: .#.#..#...#.#...##...#...#.#..##..##...
| 17: ..#...##...#.#.#.#...##...#....#...#...
| 18: ..##.#.#....#####.#.#.#...##...##..##..
| 19: .#..###.#..#.#.#######.#.#.#..#.#...#..
| 20: .#....##....#####...#######....#.#..##.

The generation is shown along the left, where 0 is the initial state. The pot
numbers are shown along the top, where 0 labels the center pot,
negative-numbered pots extend to the left, and positive pots extend toward the
right. Remember, the initial state begins at pot 0, which is not the leftmost
pot used in this example.

After one generation, only seven plants remain. The one in pot 0 matched the
rule looking for ..#.., the one in pot 4 matched the rule looking for .#.#.,
pot 9 matched .##.., and so on.

In this example, after 20 generations, the pots shown as # contain plants, the
furthest left of which is pot -2, and the furthest right of which is pot 34.
Adding up all the numbers of plant-containing pots after the 20th generation
produces 325.

After 20 generations, what is the sum of the numbers of all pots which contain
a plant?
-}

import Data.Map(Map,empty,findWithDefault,fromList,insert)

-- If ..... => # and ##### => . then there could be infinite number of pots
-- with plants after an odd number of generations.

parse :: String -> (Map String Char,(Int,Char,String,Char))
parse s = (fromList (toRules rules),(0,'.',state,'.'))
  where
    ("initial":"state:":state:rules) = words s
    toRules (src:"=>":[dest]:rest) = (src,dest) : toRules rest
    toRules _ = []

trim :: (Int,Char,String,Char) -> (Int,Char,String,Char)
trim (i,left,pots,right) = (i+length ltrim,left,reverse rpots,right)
  where
    (ltrim,pots2) = span (== left) pots
    rpots = dropWhile (== right) (reverse pots2)

next :: Map String Char -> (Int,Char,String,Char) -> (Int,Char,String,Char)
next rules (i,l,pots,r) = (i-2,newpot [l,l,l,l,l],newpots,newpot [r,r,r,r,r])
  where
    newpots = makeNew (l:l:l:l:pots ++ [r,r,r,r])
    makeNew (p1:rest@(p2:p3:p4:p5:_)) = newpot [p1,p2,p3,p4,p5] : makeNew rest
    makeNew _ = []
    newpot llcrr = findWithDefault '.' llcrr rules

plantNumbers :: (Int,Char,String,Char) -> [Int]
plantNumbers (i,_,pots,_) = map fst $ filter ((== '#') . snd) $ zip [i..] pots

testData :: String
testData = "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"

test :: ()
test
  | sum (plantNumbers $ head $ drop 20 $ iterate (trim . next rules) state) /= 325 = error "a"
  | otherwise = ()
  where (rules,state) = parse testData

part1 :: IO Int
part1 = do
  (rules,state) <- fmap parse $ readFile "input/12.txt"
  return $ sum $ plantNumbers $ head $ drop 20 $ iterate (trim . next rules) state

-- For part 2, look for cycle, or constant growth factor.

-- For my input data, after 153 steps, 53 plants remain in 178 consecutive
-- pots, moving right by one pot with each step.  At 153 steps, the sum of the
-- pot numbers is 8575.

part2 :: Integer -> Integer
part2 n = 8575 + 53*(n - 153)
