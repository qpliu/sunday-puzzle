{-
--- Day 3: No Matter How You Slice It ---

The Elves managed to locate the chimney-squeeze prototype fabric for Santa's
suit (thanks to someone who helpfully wrote its box IDs on the wall of the
warehouse in the middle of the night). Unfortunately, anomalies are still
affecting them - nobody can even agree on how to cut the fabric.

The whole piece of fabric they're working on is a very large square - at least
1000 inches on each side.

Each Elf has made a claim about which area of fabric would be ideal for Santa's
suit. All claims have an ID and consist of a single rectangle with edges
parallel to the edges of the fabric. Each claim's rectangle is defined as
follows:

 - The number of inches between the left edge of the fabric and the left edge
   of the rectangle.
 - The number of inches between the top edge of the fabric and the top edge of
   the rectangle.
 - The width of the rectangle in inches.
 - The height of the rectangle in inches.

A claim like #123 @ 3,2: 5x4 means that claim ID 123 specifies a rectangle 3
inches from the left edge, 2 inches from the top edge, 5 inches wide, and 4
inches tall. Visually, it claims the square inches of fabric represented by #
(and ignores the square inches of fabric represented by .) in the diagram
below:

| ...........
| ...........
| ...#####...
| ...#####...
| ...#####...
| ...#####...
| ...........
| ...........
| ...........

The problem is that many of the claims overlap, causing two or more claims to
cover part of the same areas. For example, consider the following claims:

| #1 @ 1,3: 4x4
| #2 @ 3,1: 4x4
| #3 @ 5,5: 2x2

Visually, these claim the following areas:

| ........
| ...2222.
| ...2222.
| .11XX22.
| .11XX22.
| .111133.
| .111133.
| ........

The four square inches marked with X are claimed by both 1 and 2. (Claim 3,
while adjacent to the others, does not overlap either of them.)

If the Elves all proceed with their own plans, none of them will have enough
fabric. How many square inches of fabric are within two or more claims?
-}

import Data.Char(isDigit)
import Data.List(nub)
import Data.Map(Map,alter,elems,empty)

parse :: String -> [(String,(Int,Int),(Int,Int))]
parse = p . words
  where
    p (elf:"@":xy:wh:rest) = (elf,pxy xy,pxy wh) : p rest
    p _ = []
    pxy xy = (read x,read (takeWhile isDigit $ tail y))
      where (x,y) = span isDigit xy

mapClaim :: (String,(Int,Int),(Int,Int)) -> Map (Int,Int) [String]
                                         -> Map (Int,Int) [String]
mapClaim (elf,(x,y),(w,h)) claimMap =
    foldr addClaim claimMap [(i,j) | i <- [x..x+w-1], j <- [y..y+h-1]]
  where addClaim xy cm = alter (Just . maybe [elf] (elf:)) xy cm

mapClaims :: [(String,(Int,Int),(Int,Int))] -> Map (Int,Int) [String]
mapClaims claims = foldr mapClaim empty claims

testData :: String
testData = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

test :: ()
test
  | length (filter ((> 1) . length) $ elems $ mapClaims $ parse testData) /= 4 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter ((> 1) . length) . elems . mapClaims . parse) $ readFile "input/03.txt"

nonOverlapping :: Map a [String] -> [String]
nonOverlapping claims =
    filter noOverlap $ map head $ filter ((== 1) . length) distinct
  where
    distinct = nub $ elems claims
    noOverlap claim = filter (claim `elem`) distinct == [[claim]]

test2 :: ()
test2
  | nonOverlapping (mapClaims $ parse testData) /= ["#3"] = error "a"
  | otherwise = ()

part2 :: IO [String]
part2 = fmap (nonOverlapping . mapClaims . parse) $ readFile "input/03.txt"
