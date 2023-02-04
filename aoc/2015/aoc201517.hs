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
