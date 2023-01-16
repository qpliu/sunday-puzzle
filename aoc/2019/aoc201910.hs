{-
--- Day 10: Monitoring Station ---

You fly into the asteroid belt and reach the Ceres monitoring station. The
Elves here have an emergency: they're having trouble tracking all of the
asteroids and can't be sure they're safe.

The Elves would like to build a new monitoring station in a nearby area of
space; they hand you a map of all of the asteroids in that region (your puzzle
input).

The map indicates whether each position is empty (.) or contains an asteroid
(#). The asteroids are much smaller than they appear on the map, and every
asteroid is exactly in the center of its marked position. The asteroids can be
described with X,Y coordinates where X is the distance from the left edge and Y
is the distance from the top edge (so the top-left corner is 0,0 and the
position immediately to its right is 1,0).

Your job is to figure out which asteroid would be the best place to build a new
monitoring station. A monitoring station can detect any asteroid to which it
has direct line of sight - that is, there cannot be another asteroid exactly
between them. This line of sight can be at any angle, not just lines aligned to
the grid or diagonally. The best location is the asteroid that can detect the
largest number of other asteroids.

For example, consider the following map:

| .#..#
| .....
| #####
| ....#
| ...##

The best location for a new monitoring station on this map is the highlighted
asteroid at 3,4 because it can detect 8 asteroids, more than any other
location. (The only asteroid it cannot detect is the one at 1,0; its view of
this asteroid is blocked by the asteroid at 2,2.) All other asteroids are worse
locations; they can detect 7 or fewer other asteroids. Here is the number of
other asteroids a monitoring station on each asteroid could detect:

| .7..7
| .....
| 67775
| ....7
| ...87

Here is an asteroid (#) and some examples of the ways its line of sight might
be blocked. If there were another asteroid at the location of a capital letter,
the locations marked with the corresponding lowercase letter would be blocked
and could not be detected:

| #.........
| ...A......
| ...B..a...
| .EDCG....a
| ..F.c.b...
| .....c....
| ..efd.c.gb
| .......c..
| ....f...c.
| ...e..d..c

Here are some larger examples:

 - Best is 5,8 with 33 other asteroids detected:

   | ......#.#.
   | #..#.#....
   | ..#######.
   | .#.#.###..
   | .#..#.....
   | ..#....#.#
   | #..#....#.
   | .##.#..###
   | ##...#..#.
   | .#....####

 - Best is 1,2 with 35 other asteroids detected:

   | #.#...#.#.
   | .###....#.
   | .#....#...
   | ##.#.#.#.#
   | ....#.#.#.
   | .##..###.#
   | ..#...##..
   | ..##....##
   | ......#...
   | .####.###.

 - Best is 6,3 with 41 other asteroids detected:

   | .#..#..###
   | ####.###.#
   | ....###.#.
   | ..###.##.#
   | ##.##.#.#.
   | ....###..#
   | ..#.#..#.#
   | #..#.#.###
   | .##...##.#
   | .....#.#..

 - Best is 11,13 with 210 other asteroids detected:

   | .#..##.###...#######
   | ##.############..##.
   | .#.######.########.#
   | .###.#######.####.#.
   | #####.##.#.##.###.##
   | ..#####..#.#########
   | ####################
   | #.####....###.#.#.##
   | ##.#################
   | #####.##.###..####..
   | ..######..##.#######
   | ####.##.####...##..#
   | .#####..#.######.###
   | ##...#.##########...
   | #.##########.#######
   | .####.#.###.###.#.##
   | ....##.##.###..#####
   | .#.#.###########.###
   | #.#.#.#####.####.###
   | ###.##.####.##.#..##

Find the best location for a new monitoring station. How many other asteroids
can be detected from that location?
-}

import Control.Monad.State(State,get,put,runState)
import Data.List(sort)
import Data.Map(Map,empty,fromList,insert,keys,member,toList,(!))
import Data.Set(Set)
import Data.Tuple(swap)

type Asteroids = ((Int,Int),Map (Int,Int) (Map (Int,Int) Int))
-- xmax, ymax, map of x,y to asteroid's view
-- asteroid's view is map of every other asteroid x,y to
-- 1 if can be monitored, 0 if occluded

parse :: String -> Asteroids
parse = p 0 0 0 0 []
  where
    p x y xmax ymax list [] = ((xmax,ymax),fromList list)
    p x y xmax ymax list ('\n':rest) = p 0 (y+1) xmax ymax list rest
    p x y xmax ymax list ('#':rest) =
        p (x+1) y (max xmax x) (max y ymax) (((x,y),empty):list) rest
    p x y xmax ymax list (_:rest) = p (x+1) y xmax ymax list rest

initViews :: Asteroids -> Asteroids
initViews asteroids = snd $ runState init asteroids
  where
    init :: State Asteroids ()
    init = do
        -- O(N^2), but lines of sights should be O(N)
        -- Still a bit slow for the 20x20 example, so it will probably be
        -- super slow for my actual input data.
        -- My actual input data was 36x36 and it wasn't much slower
        -- despite having 320 asteroids to the 20x20 examples' 300.
        as <- fmap (keys . snd) get
        initAsteroids as

    initAsteroids :: [(Int,Int)] -> State Asteroids ()
    initAsteroids [] = return ()
    initAsteroids (a:as) = do
        mapM_ (initPair a) as -- O(N^2)
        initAsteroids as

    initPair :: (Int,Int) -> (Int,Int) -> State Asteroids ()
    initPair xy0@(x0,y0) xy1@(x1,y1) = do
        done <- alreadyDone xy0 xy1
        if done then
            return ()
        else do
            let f = gcd (abs (x1-x0)) (abs (y1-y0))
            let dxy = ((x1-x0) `div` f,(y1-y0) `div` f)
            line <- getLine xy0 dxy
            initLine line line

    alreadyDone :: (Int,Int) -> (Int,Int) -> State Asteroids Bool
    alreadyDone xy0 xy1 = do
        view <- fmap ((!xy0) . snd) get
        return $ member xy1 view

    getLine :: (Int,Int) -> (Int,Int) -> State Asteroids [(Int,Int)]
    getLine xy0 (dx,dy) = do
        ((xmax,ymax),asteroids) <- get
        let inBounds (x,y) = x >= 0 && x <= xmax && y >= 0 && y <= ymax
        let dec (x,y) = (x-dx,y-dy)
        let xystart = head $ dropWhile inBounds $ iterate dec xy0
        let inc (x,y) = (x+dx,y+dy)
        return $ filter (`member` asteroids) $ takeWhile inBounds $ iterate inc (inc xystart)

    initLine :: [(Int,Int)] -> [(Int,Int)] -> State Asteroids ()
    initLine allAs (a:as@(b:_)) = do
        mapM_ (initUnviewableFrom a) allAs
        mapM_ (flip initUnviewableFrom a) allAs
        mapM_ (initUnviewableFrom b) allAs
        mapM_ (flip initUnviewableFrom b) allAs
        setViewableFrom a b
        setViewableFrom b a
        initLine allAs as
    initLine _ _ = return ()

    initUnviewableFrom :: (Int,Int) -> (Int,Int) -> State Asteroids ()
    initUnviewableFrom xy0 xy1
      | xy0 == xy1 = return ()
      | otherwise = do
        (xymax,asteroids) <- get
        let view = asteroids!xy0
        if member xy1 view then
            return ()
        else
            put (xymax,insert xy0 (insert xy1 0 view) asteroids)

    setViewableFrom :: (Int,Int) -> (Int,Int) -> State Asteroids ()
    setViewableFrom xy0 xy1
      | xy0 == xy1 = return ()
      | otherwise = do
        (xymax,asteroids) <- get
        let view = asteroids!xy0
        put (xymax,insert xy0 (insert xy1 1 view) asteroids)

findBest :: Asteroids -> (Int,(Int,Int))
findBest = maximum . map (swap . fmap sum) . toList . snd

testData :: [((Int,(Int,Int)),String)]
testData = [
        ((8,(3,4)),".#..#\n.....\n#####\n....#\n...##"),
        ((33,(5,8)),"......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"),
        ((35,(1,2)),"#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###."),
        ((41,(6,3)),".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."),
        ((210,(11,13)),".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
    ]

test :: ()
test
  | any (uncurry (/=) . fmap (findBest . initViews . parse)) testData = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . findBest . initViews . parse) $ readFile "input/10.txt"

vaporize :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
vaporize xy0@(x0,y0) asteroids = zap (4,0) [] $ sort $ map (getHeading xy0) $ filter (/= xy0) asteroids
  where
    zap _ [] [] = []
    zap _ occluded [] = zap (4,0) [] (reverse occluded)
    zap lastHeading occluded (item@((heading,_),xy):rest)
      | heading == lastHeading = zap lastHeading (item:occluded) rest
      | otherwise = xy : zap heading occluded rest

getHeading :: (Int,Int) -> (Int,Int) -> (((Int,Rational),Int),(Int,Int))
getHeading (x0,y0) xy@(x,y) = (((quadrant,slope),dy),xy)
  where
    (quadrant,dx,dy)
      | x-x0 >= 0 && y-y0 < 0 = (0,x-x0,y0-y)
      | x-x0 > 0 && y-y0 >= 0 = (1,y-y0,x-x0)
      | x-x0 <= 0 && y-y0 > 0 = (2,x0-x,y-y0)
      | otherwise = (3,y0-y,x0-x)
    slope = fromIntegral dx/fromIntegral dy

run2 :: String -> [(Int,Int)]
run2 input = vaporize xy (keys asteroids)
  where
    parsed@(_,asteroids) = parse input
    (_,xy) = findBest $ initViews parsed

test2 :: ()
test2
  | zapped !! 0 /= (11,12) = error "a"
  | zapped !! 1 /= (12,1) = error "b"
  | zapped !! 2 /= (12,2) = error "c"
  | zapped !! 9 /= (12,8) = error "d"
  | zapped !! 19 /= (16,0) = error "e"
  | zapped !! 49 /= (16,9) = error "f"
  | zapped !! 99 /= (10,16) = error "g"
  | zapped !! 198 /= (9,6) = error "h"
  | zapped !! 199 /= (8,2) = error "i"
  | zapped !! 200 /= (10,9) = error "j"
  | zapped !! 298 /= (11,1) = error "k"
  | length zapped /= 299 = error "l"
  | otherwise = ()
  where zapped = run2 $ snd $ head $ drop 4 testData

part2 :: IO Int
part2 = fmap ((\ (x,y) -> x*100+y) . (!!199) . run2) $ readFile "input/10.txt"
