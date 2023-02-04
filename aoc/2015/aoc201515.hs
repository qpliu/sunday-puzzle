parse :: String -> [Int]
parse s = parseWords $ words $ dropWhile (/= ':') s
  where
    parseWords (_:_:cap:_:dur:_:flav:_:tex:_) = [parseNum cap,parseNum dur,parseNum flav,parseNum tex]
    parseNum num = read $ takeWhile (/= ',') num

-- This algorithm might be too slow for the actual input.
bestScore :: [[Int]] -> (Int,[Int])
bestScore ingredients = s ingredients 100 []
  where
    s [] _ _ = error "this cannot happen"
    s _ 0 reverseAmounts = (score reverseAmounts,reverse reverseAmounts)
    s [i] n reverseAmounts = (score (n:reverseAmounts),reverse (n:reverseAmounts))
    s (i:is) n reverseAmounts = maximum [s is (n-m) (m:reverseAmounts) | m <- [0..n]]
    score reverseAmounts = combine 1 $ zipWith add (reverse reverseAmounts) ingredients
    add amount ingredient = map (amount*) ingredient
    combine runningTotal properties
      | null (head properties) = runningTotal
      | nextProperty > 0 = combine (runningTotal*nextProperty) (map tail properties)
      | otherwise = 0
      where
        nextProperty = sum (map head properties)

test :: ()
test
  | parse "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8" /= [-1,-2,6,3] = error "a"
  | (bestScore $ map parse $ lines "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8\nCinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3") /= (62842880,[44,56]) = error "b"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (fst . bestScore . map parse . lines) (readFile "input/15.txt")

parse2 :: String -> ([Int],Int)
parse2 s = parseWords $ words $ dropWhile (/= ':') s
  where
    parseWords (_:_:cap:_:dur:_:flav:_:tex:_:cals:_) = ([parseNum cap,parseNum dur,parseNum flav,parseNum tex],parseNum cals)
    parseNum num = read $ takeWhile (/= ',') num

bestScore2 :: [([Int],Int)] -> (Int,[Int])
bestScore2 ingredients = s ingredients 100 []
  where
    s [] _ _ = error "this cannot happen"
    s _ 0 reverseAmounts = (score reverseAmounts,reverse reverseAmounts)
    s [i] n reverseAmounts = (score (n:reverseAmounts),reverse (n:reverseAmounts))
    s (i:is) n reverseAmounts = maximum [s is (n-m) (m:reverseAmounts) | m <- [0..n]]
    score reverseAmounts = mix $ zipWith add (reverse reverseAmounts) ingredients
    add amount (ingredient,cals) = (map (amount*) ingredient,amount*cals)
    mix sums
      | sum (map snd sums) /= 500 = 0
      | otherwise = combine 1 (map fst sums)
    combine runningTotal properties
      | null (head properties) = runningTotal
      | nextProperty > 0 = combine (runningTotal*nextProperty) (map tail properties)
      | otherwise = 0
      where
        nextProperty = sum (map head properties)

part2 :: IO Int
part2 = fmap (fst . bestScore2 . map parse2 . lines) (readFile "input/15.txt")
