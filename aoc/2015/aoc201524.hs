parse :: String -> [Int]
parse s = map read $ words s

metric :: [Int] -> (Int,Int)
metric group = (length group,product group)

groups :: Int -> [Int] -> [[Int]]
groups 0 _ = [[]]
groups _ [] = []
groups total (p:ps)
  | p > total = groups total ps
  | otherwise = groups total ps ++ map (p:) (groups (total-p) ps)

ideal :: [Int] -> (Int,Int)
ideal list = minimum $ map metric $ groups (sum list `div` 3) list

test
  | ideal [1,2,3,4,5,7,8,9,10,11] /= (2,99) = error "a"
  | metric [11,9] /= (2,99) = error "b"
  | metric [10,9,1] /= (3,90) = error "c"
  | metric [11,9] /= (2,99) = error "d"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (snd . ideal . parse) (readFile "input/24.txt")

part2ideal :: [Int] -> (Int,Int)
part2ideal list = minimum $ map metric $ groups (sum list `div` 4) list

part2 :: IO Int
part2 = fmap (snd . part2ideal . parse) (readFile "input/24.txt")
