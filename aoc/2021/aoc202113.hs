{-
--- Day 13: Transparent Origami ---

You reach another volcanically active part of the cave. It would be nice if you
could do some kind of thermal imaging so you could tell ahead of time which
caves are too hot to safely enter.

Fortunately, the submarine seems to be equipped with a thermal camera! When you
activate it, you are greeted with:

| Congratulations on your purchase! To activate this infrared thermal imaging
| camera system, please enter the code found on page 1 of the manual.

Apparently, the Elves have never used this feature. To your surprise, you
manage to find the manual; as you go to open it, page 1 falls out. It's a large
sheet of transparent paper! The transparent paper is marked with random dots
and includes instructions on how to fold it up (your puzzle input). For
example:

| 6,10
| 0,14
| 9,10
| 0,3
| 10,4
| 4,11
| 6,0
| 6,12
| 4,1
| 0,13
| 10,12
| 3,4
| 3,0
| 8,4
| 1,10
| 2,14
| 8,10
| 9,0
| 
| fold along y=7
| fold along x=5

The first section is a list of dots on the transparent paper. 0,0 represents
the top-left coordinate. The first value, x, increases to the right. The second
value, y, increases downward. So, the coordinate 3,0 is to the right of 0,0,
and the coordinate 0,7 is below 0,0. The coordinates in this example form the
following pattern, where # is a dot on the paper and . is an empty, unmarked
position:

| ...#..#..#.
| ....#......
| ...........
| #..........
| ...#....#.#
| ...........
| ...........
| ...........
| ...........
| ...........
| .#....#.##.
| ....#......
| ......#...#
| #..........
| #.#........

Then, there is a list of fold instructions. Each instruction indicates a line
on the transparent paper and wants you to fold the paper up (for horizontal
y=... lines) or left (for vertical x=... lines). In this example, the first
fold instruction is fold along y=7, which designates the line formed by all of
the positions where y is 7 (marked here with -):

| ...#..#..#.
| ....#......
| ...........
| #..........
| ...#....#.#
| ...........
| ...........
| -----------
| ...........
| ...........
| .#....#.##.
| ....#......
| ......#...#
| #..........
| #.#........

Because this is a horizontal line, fold the bottom half up. Some of the dots
might end up overlapping after the fold is complete, but dots will never appear
exactly on a fold line. The result of doing this fold looks like this:

| #.##..#..#.
| #...#......
| ......#...#
| #...#......
| .#.#..#.###
| ...........
| ...........

Now, only 17 dots are visible.

Notice, for example, the two dots in the bottom left corner before the
transparent paper is folded; after the fold is complete, those dots appear in
the top left corner (at 0,0 and 0,1). Because the paper is transparent, the dot
just below them in the result (at 0,3) remains visible, as it can be seen
through the transparent paper.

Also notice that some dots can end up overlapping; in this case, the dots
merge together and become a single dot.

The second fold instruction is fold along x=5, which indicates this line:

| #.##.|#..#.
| #...#|.....
| .....|#...#
| #...#|.....
| .#.#.|#.###
| .....|.....
| .....|.....

Because this is a vertical line, fold left:

| #####
| #...#
| #...#
| #...#
| #####
| .....
| .....

The instructions made a square!

The transparent paper is pretty big, so for now, focus on just completing the
first fold. After the first fold in the example above, 17 dots are visible -
dots that end up overlapping after the fold is completed count as a single dot.

How many dots are visible after completing just the first fold instruction on
your transparent paper?
-}

import Data.Set(Set,fromList,member,partition,size,union)
import qualified Data.Set

parse :: String -> (Set (Int,Int),[(Char,Int)])
parse str = (fromList $ map toXY coords,map getFold (drop 1 folds))
  where
    (coords,folds) = span (/= "") (lines str)
    toXY coord = read ("("++coord++")")
    getFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':axis:'=':n) = (axis,read n)

doFold :: Set (Int,Int) -> (Char,Int) -> Set (Int,Int)
doFold paper ('x',n) = union left (Data.Set.map fold right)
  where
    (left,right) = partition ((< n) . fst) paper
    fold (x,y) = (2*n-x,y)
doFold paper ('y',n) = union top (Data.Set.map fold bottom)
  where
    (top,bottom) = partition ((< n) . snd) paper
    fold (x,y) = (x,2*n-y)

testData :: String
testData = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n"

test :: ()
test
  | size (doFold paper (head folds)) /= 17 = error "a"
  | otherwise = ()
  where (paper,folds) = parse testData

part1 :: IO Int
part1 = do
    (paper,folds) <- fmap parse $ readFile "input/13.txt"
    return $ size $ doFold paper (head folds)

doFolds :: (Set (Int,Int),[(Char,Int)]) -> Set (Int,Int)
doFolds (paper,folds) = foldl doFold paper folds

display :: Set (Int,Int) -> String
display paper = unlines [[if member (x,y) paper then '#' else '.' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    xmin = minimum $ Data.Set.map fst paper
    xmax = maximum $ Data.Set.map fst paper
    ymin = minimum $ Data.Set.map snd paper
    ymax = maximum $ Data.Set.map snd paper

test2 :: IO ()
test2 = (putStr . display . doFolds . parse) testData

part2 :: IO ()
part2 = readFile "input/13.txt" >>= putStr . display . doFolds . parse
