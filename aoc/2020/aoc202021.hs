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
