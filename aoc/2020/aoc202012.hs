{-
--- Day 12: Rain Risk ---

Your ferry made decent progress toward the island, but the storm came in faster
than anyone expected. The ferry needs to take evasive actions!

Unfortunately, the ship's navigation computer seems to be malfunctioning;
rather than giving a route directly to safety, it produced extremely circuitous
instructions. When the captain uses the PA system to ask if anyone can help,
you quickly volunteer.

The navigation instructions (your puzzle input) consists of a sequence of
single-character actions paired with integer input values. After staring at
them for a few minutes, you work out what they probably mean:

 - Action N means to move north by the given value.
 - Action S means to move south by the given value.
 - Action E means to move east by the given value.
 - Action W means to move west by the given value.
 - Action L means to turn left the given number of degrees.
 - Action R means to turn right the given number of degrees.
 - Action F means to move forward by the given value in the direction the ship
   is currently facing.

The ship starts by facing east. Only the L and R actions change the direction
the ship is facing. (That is, if the ship is facing east and the next
instruction is N10, the ship would move north 10 units, but would still move
east if the following action were F.)

For example:

| F10
| N3
| F7
| R90
| F11

These instructions would be handled as follows:

 - F10 would move the ship 10 units east (because the ship starts by facing
   east) to east 10, north 0.
 - N3 would move the ship 3 units north to east 10, north 3.
 - F7 would move the ship another 7 units east (because the ship is still
   facing east) to east 17, north 3.
 - R90 would cause the ship to turn right by 90 degrees and face south; it
   remains at east 17, north 3.
 - F11 would move the ship 11 units south to east 17, south 8.

At the end of these instructions, the ship's Manhattan distance (sum of the
absolute values of its east/west position and its north/south position) from
its starting position is 17 + 8 = 25.

Figure out where the navigation instructions lead. What is the Manhattan
distance between that location and the ship's starting position?
-}

-- Niven's Theorem: all forward movements must occur when the net turn is
-- a multiple of 90 degrees.

-- A list of instructions collapse to a net turn, a net turn-independent
-- translation, and a net turn-dependent translation.
-- The net turn must be a multiple of 90 degrees.

-- Let north = 0 degrees, east = 90 degrees, south = 180 degrees, west = 270
parse :: String -> (Int,(Int,Int),(Int,Int))
parse = p (0,(0,0),(0,0)) . words
  where
    p result [] = result
    p (dir,(ax,ay),r) (('N':n):rest) = p (dir,(ax,ay-read n),r) rest
    p (dir,(ax,ay),r) (('S':n):rest) = p (dir,(ax,ay+read n),r) rest
    p (dir,(ax,ay),r) (('E':n):rest) = p (dir,(ax+read n,ay),r) rest
    p (dir,(ax,ay),r) (('W':n):rest) = p (dir,(ax-read n,ay),r) rest
    p (dir,a,r) (('L':n):rest) = p ((dir-read n)`mod`360,a,r) rest
    p (dir,a,r) (('R':n):rest) = p ((dir+read n)`mod`360,a,r) rest
    p (dir,a,(rx,ry)) (('F':n):rest) = p (dir,a,newr) rest
      where newr | dir == 0   = (rx,ry-read n)
                 | dir == 90  = (rx+read n,ry)
                 | dir == 180 = (rx,ry+read n)
                 | dir == 270 = (rx-read n,ry)
                 | otherwise = error (show dir)

follow :: (Int,(Int,Int)) -> (Int,(Int,Int),(Int,Int)) -> (Int,(Int,Int))
follow (dir,(x,y)) (ddir,(adx,ady),(rdx,rdy))
  | dir == 0   = ((dir+ddir)`mod`360,(x+adx+rdx,y+ady+rdy))
  | dir == 90  = ((dir+ddir)`mod`360,(x+adx-rdy,y+ady+rdx))
  | dir == 180 = ((dir+ddir)`mod`360,(x+adx-rdx,y+ady-rdy))
  | dir == 270 = ((dir+ddir)`mod`360,(x+adx+rdy,y+ady-rdx))
  | otherwise = error (show dir)

dist :: (Int,Int) -> Int
dist (x,y) = abs x + abs y

testData :: String
testData = "F10\nN3\nF7\nR90\nF11\n"

test :: ()
test
  | (follow (90,(0,0)) . parse) testData /= (180,(17,8)) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (dist . snd . follow (90,(0,0)) . parse) $ readFile "input/12.txt"

-- Turns out that all turns are multiples of 90 degrees.

parse2 :: String -> [(Char,Int)]
parse2 = map p . words
  where p (c:rest) = (c,read rest)

follow2 :: ((Int,Int),(Int,Int)) -> (Char,Int) -> ((Int,Int),(Int,Int))
follow2 ((wx,wy),(x,y)) (c,n)
  | c == 'N' = ((wx,wy-n),(x,y))
  | c == 'S' = ((wx,wy+n),(x,y))
  | c == 'E' = ((wx+n,wy),(x,y))
  | c == 'W' = ((wx-n,wy),(x,y))
  | (c == 'R' && n == 90) || (c == 'L' && n == 270) = ((-wy,wx),(x,y))
  | (c == 'L' || c == 'R') && n == 180 = ((-wx,-wy),(x,y))
  | (c == 'R' && n == 270) || (c == 'L' && n == 90) = ((wy,-wx),(x,y))
  | c == 'F' = ((wx,wy),(x+n*wx,y+n*wy))
  | otherwise = error (show (c,n))

test2 :: ()
test2
  | (foldl follow2 ((10,-1),(0,0)) . parse2) testData /= ((4,10),(214,72)) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (dist . snd . foldl follow2 ((10,-1),(0,0)) . parse2) $ readFile "input/12.txt"
