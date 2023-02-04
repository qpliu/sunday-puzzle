import Data.Char(isDigit)
import Data.List(sort)
import Data.Map(elems,fromList,(!))

data Tower = Tower String Int [Tower] deriving (Eq,Ord,Show)

totalWeight :: Tower -> Int
totalWeight (Tower _ weight subtowers) = weight + sum (map totalWeight subtowers)

towerName :: Tower -> String
towerName (Tower name _ _) = name

parse :: String -> [Tower]
parse s = elems towers
  where
    towers = fromList $ map (parseTower . words) $ lines s
    parseTower [name,weight] = (name,Tower name (read $ filter isDigit weight) [])
    parseTower (name:weight:"->":subtowers) = (name,Tower name (read $ filter isDigit weight) (map ((towers!) . (filter (/= ','))) subtowers))

testData :: [Tower]
testData = parse "pbga (66)\nxhth (57)\nebii (61)\nhavc (66)\nktlj (57)\nfwft (72) -> ktlj, cntj, xhth\nqoyq (66)\npadx (45) -> pbga, havc, qoyq\ntknk (41) -> ugml, padx, fwft\njptl (61)\nugml (68) -> gyxo, ebii, jptl\ngyxo (61)\ncntj (57)"

findBottom :: [Tower] -> Tower
findBottom towers = snd $ maximum $ zip (map totalWeight towers) towers

test :: ()
test
  | towerName (findBottom testData) /= "tknk" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap (towerName . findBottom . parse) $ readFile "input/07.txt"

balanceWeight :: Tower -> Int
balanceWeight (Tower _ _ towers)
  | length towers < 3 = 0
  | first == second = first
  | otherwise = third
  where (first:second:third:_) = sort (map totalWeight towers)

findUnbalanced :: Int -> Tower -> (Int,String)
findUnbalanced myBalanceWeight tower@(Tower _ weight towers)
  | null towers = error "null towers"
  | null unbalanced = (weight + myBalanceWeight - totalWeight tower,towerName tower)
  | otherwise = findUnbalanced subbalanceWeight (head unbalanced)
  where
    subbalanceWeight = balanceWeight tower
    unbalanced = filter ((/= subbalanceWeight) . totalWeight) towers

test2 :: ()
test2
  | findUnbalanced 0 (findBottom testData) /= (60,"ugml") = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (fst . findUnbalanced 0 . findBottom . parse) $ readFile "input/07.txt"
