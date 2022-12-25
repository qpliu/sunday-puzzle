{-
--- Day 15: Science for Hungry People ---

Today, you set out on the task of perfecting your milk-dunking cookie recipe.
All you have to do is find the right balance of ingredients.

Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a
list of the remaining ingredients you could use to finish the recipe (your
puzzle input) and their properties per teaspoon:

 - capacity (how well it helps the cookie absorb milk)
 - durability (how well it keeps the cookie intact when full of milk)
 - flavor (how tasty it makes the cookie)
 - texture (how it improves the feel of the cookie)
 - calories (how many calories it adds to the cookie)

You can only measure ingredients in whole-teaspoon amounts accurately, and you
have to be accurate so you can reproduce your results in the future. The total
score of a cookie can be found by adding up each of the properties (negative
totals become 0) and then multiplying together everything except calories.

For instance, suppose you have these two ingredients:

Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3

Then, choosing to use 44 teaspoons of butterscotch and 56 teaspoons of cinnamon
(because the amounts of each ingredient must add up to 100) would result in a
cookie with the following properties:

 - A capacity of 44*-1 + 56*2 = 68
 - A durability of 44*-2 + 56*3 = 80
 - A flavor of 44*6 + 56*-2 = 152
 - A texture of 44*3 + 56*-1 = 76

Multiplying these together (68 * 80 * 152 * 76, ignoring calories for now)
results in a total score of 62842880, which happens to be the best score
possible given these ingredients. If any properties had produced a negative
total, it would have instead become zero, causing the whole score to multiply
to zero.

Given the ingredients in your kitchen and their properties, what is the total
score of the highest-scoring cookie you can make?
-}

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
