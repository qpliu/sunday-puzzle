import Data.Char(isDigit)
import Data.Map(Map,fromList)
import qualified Data.Map

notWrong :: (Int,Map String Int) -> Bool
notWrong (_,aunt) = all test [("children",3),("cats",7),("samoyeds",2),("pomeranians",3),("akitas",0),("vizslas",0),("goldfish",5),("trees",3),("cars",2),("perfumes",1)]
  where
    test (k,v) = maybe True (== v) $ Data.Map.lookup k aunt

parse :: String -> [(Int,Map String Int)]
parse s = map (p . words) (lines s)
  where
    p (_:n:rest) = (read (filter isDigit n),fromList (props rest))
    props (name:value:rest) = (filter (/= ':') name,read (filter isDigit value)) : props rest
    props _ = []

test :: ()
test = ()

part1 :: IO [Int]
part1 = fmap (map fst . filter notWrong . parse) (readFile "input/16.txt")

part2notWrong (_,aunt) = all (test (==)) [("children",3),("samoyeds",2),("akitas",0),("vizslas",0),("cars",2),("perfumes",1)] && all (test (>)) [("cats",7),("trees",3)] && all (test (<)) [("pomeranians",3),("goldfish",5)]
  where
    test op (k,v) = maybe True (`op` v) $ Data.Map.lookup k aunt

part2 :: IO [Int]
part2 = fmap (map fst . filter part2notWrong . parse) (readFile "input/16.txt")
