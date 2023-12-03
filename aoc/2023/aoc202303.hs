import Data.Char(isDigit)
import Data.Map(Map,alter,empty,fromList)
import qualified Data.Map
import Data.Maybe(catMaybes)

parse :: String -> [(Int,[((Int,Int),Char)])]
parse input = numbers 0 0 input
  where
    symbols = fromList (collectSymbols 0 0 input)
    collectSymbols x y [] = []
    collectSymbols x y (c:cs)
      | c == '\n' = collectSymbols 0 (y+1) cs
      | c == '.' || isDigit c = collectSymbols (x+1) y cs
      | otherwise = ((x,y),c) : collectSymbols (x+1) y cs
    numbers x y [] = []
    numbers x y (c:cs)
      | c == '\n' = numbers 0 (y+1) cs
      | isDigit c = collectNumber (x+1) y [c] (catMaybes (map numberSymbols [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1)])) cs
      | otherwise = numbers (x+1) y cs
    collectNumber x y revnum syms [] = [(read (reverse revnum),syms ++ catMaybes (map numberSymbols [(x+1,y-1)]))]
    collectNumber x y revnum syms (c:cs)
      | isDigit c = collectNumber (x+1) y (c:revnum) (syms ++ catMaybes (map numberSymbols [(x,y-1),(x,y+1)])) cs
      | otherwise = (read (reverse revnum),syms ++ catMaybes (map numberSymbols [(x,y-1),(x,y),(x,y+1)])) : numbers x y (c:cs)
    numberSymbols xy = fmap ((,) xy) $ Data.Map.lookup xy symbols

result :: String -> Int
result = sum . map fst . filter (not . null . snd) . parse

testData :: String
testData = unlines [
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
    ]

test :: ()
test
  | result testData /= 4361 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/03.txt"

collect2 :: [(Int,[((Int,Int),Char)])] -> Map (Int,Int) [Int]
collect2 = foldl collectPart empty
  where
    collectPart table (number,syms) = foldl (collectSyms number) table syms
    collectSyms number table (xy,'*') = alter (Just . maybe [number] (number:)) xy table
    collectSyms _ table _ = table

ratio2 :: [Int] -> Int
ratio2 [a,b] = a*b
ratio2 _ = 0

result2 :: String -> Int
result2 = sum . Data.Map.map ratio2 . collect2 . parse

test2 :: ()
test2
  | result2 testData /= 467835 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/03.txt"
