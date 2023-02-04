import Data.Map(Map,alter,empty,fromList,member,size,toList,unionWith,(!))
import qualified Data.Map
import Data.Set(difference,union)
import qualified Data.Set

parse :: String -> Map String String
parse = fromList . map p . words
  where p str = (bbb,aaa) where (aaa,(')':bbb)) = span (/= ')') str

testData :: String
testData = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"

toIndirect :: Map String String -> Map String Int
toIndirect direct = indirect
  where
    indirect = fromList $ map makeCount $ toList direct
    makeCount (bbb,aaa)
      | not (member aaa direct) = (bbb,0)
      | otherwise = (bbb,1 + indirect!aaa)

toCounts :: String -> (Int,Int)
toCounts input = (size direct,sum indirect)
  where
    direct = parse input
    indirect = toIndirect direct

test :: ()
test
  | uncurry (+) (toCounts testData) /= 42 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (+) . toCounts) $ readFile "input/06.txt"

testData2 :: String
testData2 = "OM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN"

invert :: Map String String -> Map String [String]
invert m = foldr collect empty $ toList m
  where
    collect (bbb,aaa) inv = alter (Just . maybe [bbb] (bbb:)) aaa inv

toGraph :: Map String String -> (String,String,Map String [String])
toGraph direct = (direct!"YOU", direct!"SAN", unionWith (++) (Data.Map.map (:[]) direct) (invert direct))

search :: (String,String,Map String [String]) -> Int
search (start,end,graph) = s 0 (Data.Set.fromList [start]) (Data.Set.fromList [start])
  where
    s n seen current
      | Data.Set.member end current = n
      | otherwise = s (n+1) (union seen next) (difference next seen)
      where
        next = Data.Set.fromList $ concatMap (graph!) $ Data.Set.toList current

test2 :: ()
test2
  | (search $ toGraph $ parse testData2) /= 4 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (search . toGraph . parse) $ readFile "input/06.txt"
