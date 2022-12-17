import Data.List(sort)
import Data.Map(Map,alter,empty,fromList,toList)

hat :: Integer -> Map [Integer] Integer
hat n
  | n < 2 = error "not enough names"
  | otherwise = addName 1 (fromList [([1,0],n-1)])
  where
    addName :: Integer -> Map [Integer] Integer -> Map [Integer] Integer
    addName i names
      | i >= n = canonical names
      | otherwise = addName (i+1) $ foldl collect empty $ concatMap (addNameI n i) $ toList names
      where
        canonical :: Map [Integer] Integer ->  Map [Integer] Integer
        canonical m = foldl collect empty $ map (\ (k,v) -> (sort k,v)) $ toList m
        collect :: Map [Integer] Integer -> ([Integer],Integer) -> Map [Integer] Integer
        collect m (k,count) = alter (Just . maybe count (+count)) k m

addNameI :: Integer -> Integer -> ([Integer],Integer) -> [([Integer],Integer)]
addNameI n i (counts,coefficient) = (1:counts,coefficient*(n-i-1)):[(0:sort (addPreviousName j counts),coefficient) | j <- [1..i]]

addPreviousName :: Integer -> [Integer] -> [Integer]
addPreviousName _ [] = error "this cannot happen"
addPreviousName name (count:counts)
  | name == 0 = count+1:counts
  | otherwise = count:addPreviousName (name-1) counts
