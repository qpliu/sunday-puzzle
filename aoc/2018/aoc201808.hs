import Data.Char(isDigit)

parse :: String -> [Int]
parse [] = []
parse s = read num : parse (dropWhile (not . isDigit) rest)
  where (num,rest) = span isDigit s

data Node = Node [Node] [Int] deriving Show

makeNode :: [Int] -> (Node,[Int])
makeNode (nchild:nmeta:rest) = (Node (reverse childList) metas,afterMeta)
  where
    (childList,afterChildList) = makeChildList nchild [] rest
    (metas,afterMeta) = splitAt nmeta afterChildList
    makeChildList n childs numbers
      | n <= 0 = (childs,numbers)
      | otherwise = makeChildList (n-1) (child:childs) moreNumbers
      where (child,moreNumbers) = makeNode numbers

sumMetadata :: Node -> Int
sumMetadata (Node children meta) = sum meta + sum (map sumMetadata children)

testData :: String
testData = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

test :: ()
test
  | sumMetadata (fst $ makeNode $ parse testData) /= 138 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sumMetadata . fst . makeNode . parse) $ readFile "input/08.txt"

getValue :: Node -> Int
getValue (Node children metadata)
  | null children = sum metadata
  | otherwise = sum $ map (getValue . (children !!) . pred) $ filter (<= length children) $ filter (> 0) $ metadata

test2 :: ()
test2
  | getValue (fst $ makeNode $ parse testData) /= 66 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (getValue . fst . makeNode . parse) $ readFile "input/08.txt"
