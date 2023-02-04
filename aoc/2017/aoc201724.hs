import Data.List(partition)

parse :: String -> [(Int,Int)]
parse = map p . words
  where
    p s = (read s1,read (tail s2))
      where (s1,s2) = span (/= '/') s

valids :: Int -> [(Int,Int)] -> [[(Int,Int)]]
valids end components = v components []
  where
    v [] _ = [[]]
    v (comp@(a,b):comps) comps2
      | a == end = map (comp:) (valids b (comps++comps2)) ++ v comps (comp:comps2)
      | b == end = map (comp:) (valids a (comps++comps2)) ++ v comps (comp:comps2)
      | otherwise = v comps (comp:comps2)

strength :: [(Int,Int)] -> Int
strength = sum . map (uncurry (+))

testData :: String
testData = "0/2\n2/2\n2/3\n3/4\n3/5\n0/1\n10/1\n9/10"

test :: ()
test
  | maximum (map strength $ valids 0 $ parse testData) /= 31 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (maximum . map strength . valids 0 . parse) $ readFile "input/24.txt"

lengthStrength :: [(Int,Int)] -> (Int,Int)
lengthStrength bridge = (length bridge,strength bridge)

part2 :: IO (Int,Int)
part2 = fmap (maximum . map lengthStrength . valids 0 . parse) $ readFile "input/24.txt"
