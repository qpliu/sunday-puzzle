import Data.Char(isDigit)
import Data.List(nub)
import Data.Map(Map,alter,elems,empty)

parse :: String -> [(String,(Int,Int),(Int,Int))]
parse = p . words
  where
    p (elf:"@":xy:wh:rest) = (elf,pxy xy,pxy wh) : p rest
    p _ = []
    pxy xy = (read x,read (takeWhile isDigit $ tail y))
      where (x,y) = span isDigit xy

mapClaim :: (String,(Int,Int),(Int,Int)) -> Map (Int,Int) [String]
                                         -> Map (Int,Int) [String]
mapClaim (elf,(x,y),(w,h)) claimMap =
    foldr addClaim claimMap [(i,j) | i <- [x..x+w-1], j <- [y..y+h-1]]
  where addClaim xy cm = alter (Just . maybe [elf] (elf:)) xy cm

mapClaims :: [(String,(Int,Int),(Int,Int))] -> Map (Int,Int) [String]
mapClaims claims = foldr mapClaim empty claims

testData :: String
testData = "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2"

test :: ()
test
  | length (filter ((> 1) . length) $ elems $ mapClaims $ parse testData) /= 4 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter ((> 1) . length) . elems . mapClaims . parse) $ readFile "input/03.txt"

nonOverlapping :: Map a [String] -> [String]
nonOverlapping claims =
    filter noOverlap $ map head $ filter ((== 1) . length) distinct
  where
    distinct = nub $ elems claims
    noOverlap claim = filter (claim `elem`) distinct == [[claim]]

test2 :: ()
test2
  | nonOverlapping (mapClaims $ parse testData) /= ["#3"] = error "a"
  | otherwise = ()

part2 :: IO [String]
part2 = fmap (nonOverlapping . mapClaims . parse) $ readFile "input/03.txt"
