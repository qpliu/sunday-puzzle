import Data.Char(isDigit)
import Data.List(sort)

interp :: String -> String -> String
interp ps ('s':x) = front++back
  where
    len = length ps
    n = read x `mod` len
    (back,front) = splitAt (len-n) ps
interp ps ('x':s)
  | i == j = ps
  | otherwise = take i ps ++ ps!!j : drop (i+1) (take j ps) ++ ps!!i : drop (j+1) ps
  where
    (a,s1) = span isDigit s
    b = dropWhile (not . isDigit) s1
    [i,j] = sort [read a,read b]
interp ps ('p':a:'/':b:_) = map swap ps
  where
    swap c | c == a = b | c == b = a | otherwise = c
interp _ insn = error insn

steps :: String -> [String]
steps s | null s = [] | otherwise = step : steps (drop 1 rest)
  where (step,rest) = span (/= ',') s

test :: ()
test
  | interp "abcde" "s1" /= "eabcd" = error "a"
  | interp "eabcd" "x3/4" /= "eabdc" = error "b"
  | interp "eabdc" "pe/b" /= "baedc" = error "c"
  | otherwise = ()

part1 :: IO String
part1 = fmap (foldl interp ['a'..'p'] . steps) $ readFile "input/16.txt"

part2 :: IO String
part2 = do
    s <- fmap steps $ readFile "input/16.txt"
    let f p = foldl interp p s
    let cycle = 1 + length (takeWhile (/= ['a'..'p']) $ tail $ iterate f ['a'..'p'])
    return $ head $ drop (1000000000 `mod` cycle) $ iterate f ['a'..'p']
