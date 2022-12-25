{-
--- Day 17: No Such Thing as Too Much ---

The elves bought too much eggnog again - 150 liters this time. To fit it all
into your refrigerator, you'll need to move it into smaller containers. You
take an inventory of the capacities of the available containers.

For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters.
If you need to store 25 liters, there are four ways to do it:

 - 15 and 10
 - 20 and 5 (the first 5)
 - 20 and 5 (the second 5)
 - 15, 5, and 5

Filling all containers entirely, how many different combinations of containers
can exactly fit all 150 liters of eggnog?
-}

combinations :: Int -> [Int] -> [[Int]]
combinations eggnog [] = []
combinations eggnog containers@(c:cs)
  | sum containers < eggnog = []
  | sum containers == eggnog = [containers]
  | c > eggnog = combinations eggnog cs
  | c == eggnog = [c] : combinations eggnog cs
  | otherwise = combinations eggnog cs ++ map (c:) (combinations (eggnog-c) cs)

parse :: String -> [Int]
parse = map read . words

test :: ()
test
  | combinations 25 [20,15,10,5,5] /= [[15,10],[15,5,5],[20,5],[20,5]] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . combinations 150 . parse) (readFile "input/17.txt")

count2 :: [Int] -> Int
count2 list = length (filter (== minimum list) list)

part2 :: IO Int
part2 = fmap (count2 . map length . combinations 150 . parse) (readFile "input/17.txt")
