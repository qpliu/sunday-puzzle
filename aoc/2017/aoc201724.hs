{-
--- Day 24: Electromagnetic Moat ---

The CPU itself is a large, black building surrounded by a bottomless pit.
Enormous metal tubes extend outward from the side of the building at regular
intervals and descend down into the void. There's no way to cross, but you need
to get inside.

No way, of course, other than building a bridge out of the magnetic components
strewn about nearby.

Each component has two ports, one on each end. The ports come in all different
types, and only matching types can be connected. You take an inventory of the
components by their port types (your puzzle input). Each port is identified by
the number of pins it uses; more pins mean a stronger connection for your
bridge. A 3/7 component, for example, has a type-3 port on one side, and a
type-7 port on the other.

Your side of the pit is metallic; a perfect surface to connect a magnetic,
zero-pin port. Because of this, the first port you use must be of type 0. It
doesn't matter what type of port you end with; your goal is just to make the
bridge as strong as possible.

The strength of a bridge is the sum of the port types in each component. For
example, if your bridge is made of components 0/3, 3/7, and 7/4, your bridge
has a strength of 0+3 + 3+7 + 7+4 = 24.

For example, suppose you had the following components:

| 0/2
| 2/2
| 2/3
| 3/4
| 3/5
| 0/1
| 10/1
| 9/10

With them, you could make the following valid bridges:

0/1
0/1--10/1
0/1--10/1--9/10
0/2
0/2--2/3
0/2--2/3--3/4
0/2--2/3--3/5
0/2--2/2
0/2--2/2--2/3
0/2--2/2--2/3--3/4
0/2--2/2--2/3--3/5

(Note how, as shown by 10/1, order of ports within a component doesn't matter.
However, you may only use each port on a component once.)

Of these bridges, the strongest one is 0/1--10/1--9/10; it has a strength of
0+1 + 1+10 + 10+9 = 31.

What is the strength of the strongest bridge you can make with the components
you have available?
-}

import Data.List(partition)

parse :: String -> [(Int,Int)]
parse = map p . words
  where
    p s = (read s1,read (tail s2))
      where (s1,s2) = span (/= '/') s

valids :: Int -> [(Int,Int)] -> [[(Int,Int)]]
valids end components = v components []
  where
    v [] _ = [[]]
    v (comp@(a,b):comps) comps2
      | a == end = map (comp:) (valids b (comps++comps2)) ++ v comps (comp:comps2)
      | b == end = map (comp:) (valids a (comps++comps2)) ++ v comps (comp:comps2)
      | otherwise = v comps (comp:comps2)

strength :: [(Int,Int)] -> Int
strength = sum . map (uncurry (+))

testData :: String
testData = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"

test :: ()
test
  | maximum (map strength $ valids 0 $ parse testData) /= 31 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maximum . map strength . valids 0 . parse) $ readFile "input/24.txt"

lengthStrength :: [(Int,Int)] -> (Int,Int)
lengthStrength bridge = (length bridge,strength bridge)

part2 :: IO (Int,Int)
part2 = fmap (maximum . map lengthStrength . valids 0 . parse) $ readFile "input/24.txt"
