module AOC202021 where

import Data.List(intercalate,partition,sort)
import Data.Map(Map,alter,elems,empty,toList)
import Data.Set(Set,delete,fromList,intersection,notMember,size,unions)
import qualified Data.Set

import AOC

aoc = AOC {
    day="../../2020/input/21",
    aocTests=[
        AOCTest {
            testData=unlines [
                "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)",
                "trh fvjkl sbzzf mxmxvkd (contains dairy)",
                "sqjhc fvjkl (contains soy)",
                "sqjhc mxmxvkd sbzzf (contains fish)"
                ],
            testResult=Just "5",
            testResult2=Just (show "mxmxvkd,sqjhc,fvjkl")
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map (p . words) . lines
  where
    p food = (fromList ingredients,map init allergens)
      where (ingredients,_:allergens) = span (/= "(contains") food

possibles :: [(Set String,[String])] -> Map String (Set String)
possibles = foldr collect empty
  where
    collect (ingredients,allergens) table = foldr collect2 table allergens
      where
        collect2 = alter (Just . maybe ingredients (intersection ingredients)) 

count :: Set String -> Set String -> Int
count possible = size . Data.Set.filter (`notMember` possible)

result input =
    sum $ map (count (unions $ elems $ possibles input)) (map fst input)

sieve :: [(String,Set String)] -> [(String,String)]
sieve = pass [] . partition ((== 1) . size . snd)
  where
    pass done ([],[]) = done
    pass done ([],possible) =
        pass done $ partition ((== 1) . size . snd) possible
    pass done ((allergen,ingredients):queue,possible) =
        pass ((allergen,ingredient):done)
             (queue,map (fmap (delete ingredient)) possible)
      where ingredient = minimum ingredients

result2 = intercalate "," . map snd . sort . sieve . toList . possibles
