import Data.Array(Array,array,bounds,inRange,range,(!),(//))
import Data.Set(Set,empty,insert,member)

parse :: String -> Array Int (String,Int)
parse = p 0 [] . map words . lines
  where
    p ip insns ((op:arg:_):rest) = p (ip+1) ((ip,(op,read $ dropWhile (== '+') arg)):insns) rest
    p ip insns _ = array (0,ip-1) insns

run1 :: Int -> Int -> Set Int -> Array Int (String,Int) -> Int
run1 ip acc seen code
  | ip `member` seen = acc
  | not (inRange (bounds code) ip) = error (show (ip,bounds code))
  | otherwise = execute (code!ip)
  where
    execute ("acc",n) = run1 (ip+1) (acc+n) (insert ip seen) code
    execute ("jmp",n) = run1 (ip+n) acc (insert ip seen) code
    execute _ = run1 (ip+1) acc (insert ip seen) code

testData :: String
testData = "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6\n"

test :: ()
test
  | (run1 0 0 empty . parse) testData /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (run1 0 0 empty . parse) $ readFile "input/08.txt"

run2 :: Int -> Int -> Set Int -> Array Int (String,Int) -> Maybe Int
run2 ip acc seen code
  | ip `member` seen = Nothing
  | not (inRange (bounds code) ip) = Just acc
  | otherwise = execute (code!ip)
  where
    execute ("acc",n) = run2 (ip+1) (acc+n) (insert ip seen) code
    execute ("jmp",n) = run2 (ip+n) acc (insert ip seen) code
    execute _ = run2 (ip+1) acc (insert ip seen) code

try2 :: Int -> Array Int (String,Int) -> Int
try2 ip code
  | op == "jmp" =
      maybe (try2 (ip+1) code) id $ run2 0 0 empty (code // [(ip,("nop",n))])
  | op == "nop" =
      maybe (try2 (ip+1) code) id $ run2 0 0 empty (code // [(ip,("jmp",n))])
  | otherwise = try2 (ip+1) code
  where (op,n) = code!ip

test2 :: ()
test2
  | (try2 0 . parse) testData /= 8 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (try2 0 . parse) $ readFile "input/08.txt"
