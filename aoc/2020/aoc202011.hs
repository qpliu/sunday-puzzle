{-
--- Day 11: Seating System ---

Your plane lands with plenty of time to spare. The final leg of your journey is
a ferry that goes directly to the tropical island where you can finally start
your vacation. As you reach the waiting area to board the ferry, you realize
you're so early, nobody else has even arrived yet!

By modeling the process people use to choose (or abandon) their seat in the
waiting area, you're pretty sure you can predict the best place to sit. You
make a quick map of the seat layout (your puzzle input).

The seat layout fits neatly on a grid. Each position is either floor (.), an
empty seat (L), or an occupied seat (#). For example, the initial seat layout might look like this:

| L.LL.LL.LL
| LLLLLLL.LL
| L.L.L..L..
| LLLL.LL.LL
| L.LL.LL.LL
| L.LLLLL.LL
| ..L.L.....
| LLLLLLLLLL
| L.LLLLLL.L
| L.LLLLL.LL

Now, you just need to model the people who will be arriving shortly.
Fortunately, people are entirely predictable and always follow a simple set of
rules. All decisions are based on the number of occupied seats adjacent to a
given seat (one of the eight positions immediately up, down, left, right, or
diagonal from the seat). The following rules are applied to every seat
simultaneously:

 - If a seat is empty (L) and there are no occupied seats adjacent to it, the
   seat becomes occupied.
 - If a seat is occupied (#) and four or more seats adjacent to it are also
   occupied, the seat becomes empty.
 - Otherwise, the seat's state does not change.

Floor (.) never changes; seats don't move, and nobody sits on the floor.

After one round of these rules, every seat in the example layout becomes occupied:

| #.##.##.##
| #######.##
| #.#.#..#..
| ####.##.##
| #.##.##.##
| #.#####.##
| ..#.#.....
| ##########
| #.######.#
| #.#####.##

After a second round, the seats with four or more occupied adjacent seats
become empty again:

| #.LL.L#.##
| #LLLLLL.L#
| L.L.L..L..
| #LLL.LL.L#
| #.LL.LL.LL
| #.LLLL#.##
| ..L.L.....
| #LLLLLLLL#
| #.LLLLLL.L
| #.#LLLL.##

This process continues for three more rounds:

| #.##.L#.##
| #L###LL.L#
| L.#.#..#..
| #L##.##.L#
| #.##.LL.LL
| #.###L#.##
| ..#.#.....
| #L######L#
| #.LL###L.L
| #.#L###.##

| #.#L.L#.##
| #LLL#LL.L#
| L.L.L..#..
| #LLL.##.L#
| #.LL.LL.LL
| #.LL#L#.##
| ..L.L.....
| #L#LLLL#L#
| #.LLLLLL.L
| #.#L#L#.##

| #.#L.L#.##
| #LLL#LL.L#
| L.#.L..#..
| #L##.##.L#
| #.#L.LL.LL
| #.#L#L#.##
| ..L.L.....
| #L#L##L#L#
| #.LLLLLL.L
| #.#L#L#.##

At this point, something interesting happens: the chaos stabilizes and further
applications of these rules cause no seats to change state! Once people stop
moving around, you count 37 occupied seats.

Simulate your seating area by applying the seating rules repeatedly until no
seats change state. How many seats end up occupied?
-}

import Data.Map(Map,findWithDefault,fromList,keys,mapWithKey,member,(!))

parse :: String -> Map (Int,Int) Int
parse = fromList . p 0 0
  where
    p x y [] = []
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y ('L':rest) = ((x,y),0) : p (x+1) y rest
    p x y ('#':rest) = ((x,y),1) : p (x+1) y rest
    p x y (_:rest) = p (x+1) y rest

step :: Map (Int,Int) Int -> Map (Int,Int) Int
step seats = mapWithKey update seats
  where
    update (x,y) state
      | neighbors == 0 = 1
      | neighbors >= 4 = 0
      | otherwise = state
      where
        neighbors = sum $ [findWithDefault 0 (x+dx,y+dy) seats | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

fixed :: Eq a => (a -> a) -> a -> a
fixed f input
  | input == output = input
  | otherwise = fixed f output
  where output = f input

testData :: [String]
testData = [
    "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n",
    "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n",
    "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##\n",
    "#.##.L#.##\n#L###LL.L#\nL.#.#..#..\n#L##.##.L#\n#.##.LL.LL\n#.###L#.##\n..#.#.....\n#L######L#\n#.LL###L.L\n#.#L###.##\n",
    "#.#L.L#.##\n#LLL#LL.L#\nL.L.L..#..\n#LLL.##.L#\n#.LL.LL.LL\n#.LL#L#.##\n..L.L.....\n#L#LLLL#L#\n#.LLLLLL.L\n#.#L#L#.##\n",
    "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##\n"
    ]

test :: ()
test
  | map parse testData /= (take 6 . iterate step . parse) (head testData) = error "a"
  | parse (testData !! 5) /= (fixed step . parse) (head testData) = error "b"
  | (sum . fixed step . parse) (head testData) /= 37 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . fixed step . parse) $ readFile "input/11.txt"

makeNeighbors :: Map (Int,Int) Int -> Map (Int,Int) [(Int,Int)]
makeNeighbors seats = mapWithKey neighbors seats
  where
    (xmax,ymax) = (maximum $ map fst $ keys seats,maximum $ map snd $ keys seats)
    neighbors (x,y) _ = concatMap (take 1 . filter (`member` seats)) [
        [(x+i,y) | i <- [1..xmax-x]],
        [(x-i,y) | i <- [1..x]],
        [(x,y+i) | i <- [1..ymax-y]],
        [(x,y-i) | i <- [1..y]],
        [(x+i,y+i) | i <- [1..xmax-x]],
        [(x+i,y-i) | i <- [1..xmax-x]],
        [(x-i,y+i) | i <- [1..x]],
        [(x-i,y-i) | i <- [1..x]]
        ]

step2 :: Map (Int,Int) [(Int,Int)] -> Map (Int,Int) Int -> Map (Int,Int) Int
step2 neighbors seats = mapWithKey update seats
  where
    update xy state
      | n == 0 = 1
      | n >= 5 = 0
      | otherwise = state
      where
        n = sum [findWithDefault 0 nxy seats | nxy <- neighbors!xy]

test2 :: ()
test2
  | (sum . fixed (step2 neighbors)) seats /= 26 = error "a"
  | otherwise = ()
  where
    seats = parse $ head testData
    neighbors = makeNeighbors seats

part2 :: IO Int
part2 = do
    seats <- fmap parse $ readFile "input/11.txt"
    return $ sum $ fixed (step2 (makeNeighbors seats)) seats
