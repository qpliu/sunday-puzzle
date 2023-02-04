import Data.Bits(popCount,setBit,(.|.),(.&.))
import Data.Char(ord)

parse :: String -> [[Int]]
parse = p [[]] . lines
  where
    p groups [] = groups
    p groups ("":rest) = p ([]:groups) rest
    p (group:groups) (ans:rest) = p ((foldl setBit 0 yeses:group):groups) rest
      where yeses = map ((+ (-ord 'a')) . ord) ans

compileGroup :: [Int] -> Int
compileGroup = foldr (.|.) 0

testData :: String
testData = "abc\n\na\nb\nc\n\nab\nac\n\na\na\na\na\n\nb\n"

test :: ()
test
  | (sum . map (popCount . compileGroup) . parse) testData /= 11 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (popCount . compileGroup) . parse) $ readFile "input/06.txt"

compileGroup2 :: [Int] -> Int
compileGroup2 = foldr (.&.) (2^30-1)

test2 :: ()
test2
  | (sum . map (popCount . compileGroup2) . parse) testData /= 6 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map (popCount . compileGroup2) . parse) $ readFile "input/06.txt"
