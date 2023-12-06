mapping :: [[Int]] -> Int -> Int
mapping [] src = src
mapping ([destRangeStart,srcRangeStart,rangeLen]:specs) src
  | src >= srcRangeStart && src < srcRangeStart + rangeLen = destRangeStart + src - srcRangeStart
  | otherwise = mapping specs src

parse :: String -> ([Int],[(String,(String,[[Int]]))])
parse = parseSeeds . lines
  where
    parseSeeds (l:"":ls) = (,) (map read (drop 1 (words l))) (parseMaps ls)
    parseMaps [] = []
    parseMaps (l:ls) = (from,(to,map (map read . words) specs)) : parseMaps (drop 1 rest)
      where
        (from,afterFrom) =  span (/= '-') l
        to = takeWhile (/= ' ') (drop 4 afterFrom)
        (specs,rest) = span (/= "") ls

almanac :: [(String,(String,[[Int]]))] -> String -> String -> Int -> Int
almanac assocs srcType destType src
  | srcType == destType = src
  | otherwise = almanac assocs nextType destType (mapping specs src)
  where
    Just (nextType,specs) = lookup srcType assocs

result :: String -> Int
result input = minimum (map (almanac assocs "seed" "location") seeds)
  where
    (seeds,assocs) = parse input

testData :: String
testData = unlines [
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
    ]

test :: ()
test
  | result testData /= 35 = error "a"
  | seeds /= [79,14,55,13] = error "a"
  | map (almanac assocs "seed" "soil") seeds /= [81,14,57,13] = error "a"
  | map (almanac assocs "seed" "location") seeds /= [82,43,86,35] = error "a"
  | otherwise = ()
  where
    (seeds,assocs) = parse testData

part1 :: IO Int
part1 = fmap result $ readFile "input/05.txt"

-- for part 2, work backwards

parse2 :: ([Int],[(String,(String,[[Int]]))]) -> ([(Int,Int)],[(String,([[Int]],String))])
parse2 (seeds,assocs) = (toPairs seeds,map invert assocs)
  where
    toPairs [] = []
    toPairs (a:b:c) = (a,b) : toPairs c
    invert (from,(to,specs)) = (to,(specs,from))

data Tree = Tree Int Int String Int [Tree] deriving (Eq,Ord,Show)

buildTree :: [(String,([[Int]],String))] -> Int -> Int -> String -> Int -> Tree
buildTree assocs locMin len destType destMin =
    Tree locMin len destType destMin subtrees
  where
    Just (specs,srcType) = lookup destType assocs
    [minDestRangeStart,minSrcRangeStart,minRangeLen] = minimum specs
    subtrees :: [Tree]
    subtrees
      | destType == "seed" = []
      | destMin+len <= minDestRangeStart =
          [buildTree assocs locMin len srcType destMin]
      | destMin < minDestRangeStart =
          let rangeLen = minDestRangeStart-destMin
          in  buildTree assocs locMin rangeLen srcType destMin : makeSubtrees (locMin+rangeLen) (len-rangeLen) (destMin+rangeLen) specs
      | otherwise = makeSubtrees locMin len destMin specs
    makeSubtrees :: Int -> Int -> Int -> [[Int]] -> [Tree]
    makeSubtrees locMin len destMin [] =
        [buildTree assocs locMin len srcType destMin]
    makeSubtrees locMin len destMin currentSpecs@([destRangeStart,srcRangeStart,rangeLen]:remainingSpecs)
      | destMin >= destRangeStart+rangeLen = makeSubtrees locMin len destMin remainingSpecs
      | destMin+len <= destRangeStart = makeSubtrees locMin len destMin remainingSpecs
      | preRangeLen > 0 = makeSubtrees locMin preRangeLen destMin remainingSpecs ++ makeSubtrees (locMin+preRangeLen) (len-preRangeLen) (destMin+preRangeLen) currentSpecs
      | postRangeLen > 0 = buildTree assocs locMin (len-postRangeLen) srcType (srcRangeStart-preRangeLen) : makeSubtrees (locMin+len-postRangeLen) postRangeLen (destMin+len-postRangeLen) remainingSpecs
      | otherwise = [buildTree assocs locMin len srcType (srcRangeStart-preRangeLen)]
      where
        preRangeLen = destRangeStart-destMin
        postRangeLen = destMin+len-(destRangeStart+rangeLen)

leafs :: Tree -> [Tree]
leafs tree@(Tree _ _ srcType _ subtrees)
  | srcType == "seed" = [tree]
  | otherwise = concatMap leafs subtrees

inSeedRange :: (Int,Int) -> Tree -> [Tree]
inSeedRange (seedStart,seedRange) (Tree locMin len "seed" seedMin [])
  | seedMin+len < seedStart = []
  | seedMin >= seedStart+seedRange = []
  | seedStart < seedMin = [Tree locMin (min len (seedStart+seedRange-seedMin)) "seed" seedMin []]
  | otherwise = let offset = seedStart-seedMin in [Tree (locMin+offset) (min (len-offset) (seedStart+seedRange-seedMin-offset)) "seed" (seedMin+offset) []]
inSeedRange _ _ = []

result2 :: String -> Int
result2 input = location
  where
    (seedRanges,assocs) = parse2 $ parse input
    Tree location _ "seed" _ [] = minimum $ concat [inSeedRange r t | t <- (leafs $ buildTree assocs 0 (2^40) "location" 0), r <- seedRanges]

test2 :: ()
test2
  | result2 testData /= 46 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/05.txt"
