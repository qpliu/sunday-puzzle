{-
--- Day 21: Allergen Assessment ---

You reach the train's last stop and the closest you can get to your vacation
island without getting wet. There aren't even any boats here, but nothing can
stop you now: you build a raft. You just need a few days' worth of food for
your journey.

You don't speak the local language, so you can't read any ingredients lists.
However, sometimes, allergens are listed in a language you do understand. You
should be able to use this information to determine which ingredient contains
which allergen and work out which foods are safe to take with you on your trip.

You start by compiling a list of foods (your puzzle input), one food per line.
Each line includes that food's ingredients list followed by some or all of the
allergens the food contains.

Each allergen is found in exactly one ingredient. Each ingredient contains zero
or one allergen. Allergens aren't always marked; when they're listed (as in
(contains nuts, shellfish) after an ingredients list), the ingredient that
contains each listed allergen will be somewhere in the corresponding
ingredients list. However, even if an allergen isn't listed, the ingredient
that contains that allergen could still be present: maybe they forgot to label
it, or maybe it was labeled in a language you don't know.

For example, consider the following list of foods:

| mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
| trh fvjkl sbzzf mxmxvkd (contains dairy)
| sqjhc fvjkl (contains soy)
| sqjhc mxmxvkd sbzzf (contains fish)

The first food in the list has four ingredients (written in a language you
don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might
contain other allergens, a few allergens the food definitely contains are
listed afterward: dairy and fish.

The first step is to determine which ingredients can't possibly contain any of
the allergens in any food in your list. In the above example, none of the
ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the
number of times any of these ingredients appear in any ingredients list
produces 5: they all appear once each except sbzzf, which appears twice.

Determine which ingredients cannot possibly contain any of the allergens in
your list. How many times do any of those ingredients appear?
-}

import Data.List(intercalate,sort)
import Data.Set(Set,difference,elems,fromList,intersection,member,size,unions)

type Food = (Set String,Set String) -- ingredients, allergens

parse :: String -> [Food]
parse = map (parseFood . words) . lines

parseFood :: [String] -> Food
parseFood = parseIngredients []
  where
    parseIngredients ingredients (ingredient:rest)
      | ingredient == "(contains" = parseAllergens ingredients [] rest
      | otherwise = parseIngredients (ingredient:ingredients) rest
    parseAllergens ingredients allergens (allergen:rest) = parseAllergens ingredients ((filter (/= ',') $ filter (/= ')') allergen):allergens) rest
    parseAllergens ingredients allergens [] = (fromList ingredients,fromList allergens)

allAllergens :: [Food] -> Set String
allAllergens = unions . map snd

allIngredients :: [Food] -> Set String
allIngredients = unions . map fst

containsIngredient :: String -> Food -> Bool
containsIngredient ingredient (ingredients,_) = member ingredient ingredients

nonallergenic :: [Food] -> Set String
nonallergenic foods = foldl difference ingredients [allergenCandidates allergen | allergen <- elems (allAllergens foods)]
  where
    ingredients = allIngredients foods
    allergenCandidates allergen = foldr collect ingredients foods
      where
        collect (ings,allergs) candidates
          | member allergen allergs = intersection candidates ings
          | otherwise = candidates

ingredientCount :: [Food] -> String -> Int
ingredientCount foods ingredient = length $ filter (member ingredient . fst) foods

run :: String -> Int
run input = sum [ingredientCount foods ingred | ingred <- elems (nonallergenic foods)]
  where foods = parse input

testData :: String
testData = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)\n"

test :: ()
test
  | run testData /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/21.txt"

allergens :: [Food] -> [(String,String)]
allergens foods = sort $ map (fmap minimum) $ reduce assocs
  where
    ingredients = allIngredients foods
    allergenCandidates allergen = foldr collect ingredients foods
      where
        collect (ings,allergs) candidates
          | member allergen allergs = intersection candidates ings
          | otherwise = candidates
    assocs = [(allergen,allergenCandidates allergen) | allergen <- elems (allAllergens foods)]
    reduce assocs
      | assocs == reduced = assocs
      | otherwise = reduce reduced
      where
        reduced = foldr reduceStep assocs assocs
        reduceStep allerg@(name,ings) assocs
          | size ings > 1 = assocs
          | otherwise = map rm assocs
          where rm a@(name2,ings2) | name == name2 = a | otherwise = (name2,difference ings2 ings)

test2 :: ()
test2
  | (intercalate "," . map snd . allergens . parse) testData /= "mxmxvkd,sqjhc,fvjkl" = error "a"
  | otherwise = ()

part2 :: IO String
part2 = fmap (intercalate "," . map snd . allergens . parse) $ readFile "input/21.txt"
