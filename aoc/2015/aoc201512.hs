import Data.Char(isDigit)
import Data.List(partition)

findNumbers :: String -> [Int]
findNumbers s
  | null n = []
  | otherwise = read n : findNumbers rest
  where
    (n,rest) = spanNumber $ dropWhile notNumber s
    notNumber c = c /= '-' && not (isDigit c)
    spanNumber (c:cs) = let (n,rest) = span isDigit cs in (c:n,rest)
    spanNumber _ = ("","")

test :: ()
test
  | findNumbers "[1,2,3]" /= [1,2,3] = error "a"
  | findNumbers "{\"a\":2,\"b\":4}" /= [2,4] = error "b"
  | findNumbers "[[[3]]]" /= [3] = error "c"
  | findNumbers "{\"a\":{\"b\":4},\"c\":-1}" /= [4,-1] = error "d"
  | findNumbers "{\"a\":[-1,1]}" /= [-1,1] = error "e"
  | findNumbers "[-1,{\"a\":1}]" /= [-1,1] = error "f"
  | findNumbers "[]" /= [] = error "g"
  | findNumbers "{}" /= [] = error "g"
  | otherwise = ()


main :: IO ()
main = getContents >>= print . sum . findNumbers

part1 :: IO Int
part1 = fmap (sum . findNumbers) (readFile "input/12.txt")

data Json = JNumber Int | JStr String | JArray [Json] | JObj [Json] deriving (Eq,Show)

parse :: String -> (Json,String)
parse (c:s)
  | c == '{' = parseObj s []
  | c == '[' = parseArray s []
  | c == '"' = (JStr (takeWhile (/= '"') s),drop 1 $ dropWhile (/= '"') s)
  | c == '-' = (JNumber (0 - read (takeWhile isDigit s)),dropWhile isDigit s)
  | isDigit c = (JNumber (read (c:takeWhile isDigit s)),dropWhile isDigit s)

parseObj :: String -> [Json] -> (Json,String)
parseObj (c:s) items
  | c == '}' = (JObj items,s)
  | c == ',' = parseObj s items
  | c == '"' = let (item,rest) = parse $ drop 1 $ dropWhile (/= ':') s in parseObj rest (item:items)

parseArray :: String -> [Json] -> (Json,String)
parseArray (c:s) items
  | c == ']' = (JArray items,s)
  | c == ',' = parseArray s items
  | otherwise = let (item,rest) = parse (c:s) in parseArray rest (item:items)

count :: Json -> Int
count (JNumber n) = n
count (JStr _) = 0
count (JArray a) = sum (map count a)
count (JObj o) = sum (map count o)

dropRed :: Json -> Json
dropRed (JObj o)
  | JStr "red" `elem` o = JStr "dropped"
  | otherwise = JObj (map dropRed o)
dropRed (JArray a) = JArray (map dropRed a)
dropRed a = a

part2 :: IO Int
part2 = fmap (count . dropRed . fst . parse) (readFile "input/12.txt")
