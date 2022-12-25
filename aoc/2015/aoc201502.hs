{-
--- Day 2: I Was Told There Would Be No Math ---

The elves are running low on wrapping paper, and so they need to submit an
order for more. They have a list of the dimensions (length l, width w, and
height h) of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which
makes calculating the required wrapping paper for each gift a little easier:
find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves
also need a little extra paper for each present: the area of the smallest side.

For example:

 - A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet
   of wrapping paper plus 6 square feet of slack, for a total of 58 square
   feet.
 - A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet
   of wrapping paper plus 1 square foot of slack, for a total of 43 square
   feet.

All numbers in the elves' list are in feet. How many total square feet of
wrapping paper should they order?
-}
import Data.List(sort)

parse :: String -> [Integer]
parse s = sort $ take 3 $ snd $ foldl p (0,[]) (s ++ "x")
  where
    p (acc,ls) '0' = (acc*10,ls)
    p (acc,ls) '1' = (acc*10+1,ls)
    p (acc,ls) '2' = (acc*10+2,ls)
    p (acc,ls) '3' = (acc*10+3,ls)
    p (acc,ls) '4' = (acc*10+4,ls)
    p (acc,ls) '5' = (acc*10+5,ls)
    p (acc,ls) '6' = (acc*10+6,ls)
    p (acc,ls) '7' = (acc*10+7,ls)
    p (acc,ls) '8' = (acc*10+8,ls)
    p (acc,ls) '9' = (acc*10+9,ls)
    p (acc,ls) _ = (0,acc:ls)

area :: String -> Integer
area s = 3*l*w + 2*w*h + 2*l*h
  where
    [l,w,h] = parse s

test :: ()
test
  | area "2x3x4" /= 58 = error "a"
  | area "1x1x10" /= 43 = error "b"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (sum . map area . words) (readFile "input/02.txt")

part2area :: String -> Integer
part2area s = 2*l+2*w+l*w*h
  where
    [l,w,h] = parse s

part2 :: IO Integer
part2 = fmap (sum . map part2area . words) (readFile "input/02.txt")
