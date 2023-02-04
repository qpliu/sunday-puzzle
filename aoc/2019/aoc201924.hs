import Data.Bits(clearBit,complementBit,popCount,setBit,testBit,(.&.))
import Data.Set(empty,insert,member)

parse :: String -> Int
parse = foldr parseTile 0 . zip [0..24] . filter (`elem` ".#?")
  where
    parseTile (i,ch) grid
      | ch == '#' = setBit grid i
      | otherwise = grid

display :: Int -> String
display grid = unlines [l [0..4], l [5..9], l [10..14], l [15..19], l [20..24]]
  where
    l bits = [if testBit grid bit then '#' else '.' | bit <- bits]

neighbors :: [(Int,Int)]
neighbors = [
    (0,  foldl setBit 0 [         1,  5]),
    (1,  foldl setBit 0 [     0,  2,  6]),
    (2,  foldl setBit 0 [     1,  3,  7]),
    (3,  foldl setBit 0 [     2,  4,  8]),
    (4,  foldl setBit 0 [     3,      9]),

    (5,  foldl setBit 0 [ 0,      6, 10]),
    (6,  foldl setBit 0 [ 1,  5,  7, 11]),
    (7,  foldl setBit 0 [ 2,  6,  8, 12]),
    (8,  foldl setBit 0 [ 3,  7,  9, 13]),
    (9,  foldl setBit 0 [ 4,  8,     14]),

    (10, foldl setBit 0 [ 5,     11, 15]),
    (11, foldl setBit 0 [ 6, 10, 12, 16]),
    (12, foldl setBit 0 [ 7, 11, 13, 17]),
    (13, foldl setBit 0 [ 8, 12, 14, 18]),
    (14, foldl setBit 0 [ 9, 13,     19]),

    (15, foldl setBit 0 [10,     16, 20]),
    (16, foldl setBit 0 [11, 15, 17, 21]),
    (17, foldl setBit 0 [12, 16, 18, 22]),
    (18, foldl setBit 0 [13, 17, 19, 23]),
    (19, foldl setBit 0 [14, 18,     24]),

    (20, foldl setBit 0 [15,     21    ]),
    (21, foldl setBit 0 [16, 20, 22    ]),
    (22, foldl setBit 0 [17, 21, 23    ]),
    (23, foldl setBit 0 [18, 22, 24    ]),
    (24, foldl setBit 0 [19, 23        ])
    ]

advance :: Int -> Int
advance grid = foldr advanceTile grid neighbors
  where
    advanceTile (bit,adjacent) g
      | neighborCount == 1 = setBit g bit
      | neighborCount == 2 = complementBit g bit
      | otherwise = clearBit g bit 
      where neighborCount = popCount (adjacent .&. grid)

findRepeat :: Ord a => [a] -> a
findRepeat = find empty
  where
    find set (a:as)
      | member a set = a
      | otherwise = find (insert a set) as

testData :: [String]
testData = ["....#\n#..#.\n#..##\n..#..\n#....",
            "#..#.\n####.\n###.#\n##.##\n.##..",
            "#####\n....#\n....#\n...#.\n#.###",
            "#....\n####.\n...##\n#.##.\n.##.#",
            "####.\n....#\n##..#\n.....\n##..."]

test :: ()
test
  | or (zipWith (/=) (iterate advance (parse $ head testData)) (map parse testData)) = error "a"
  | parse ".....\n.....\n.....\n#....\n.#..." /= 2129920 = error "b"
  | findRepeat (iterate advance (parse $ head testData)) /= 2129920 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (findRepeat . iterate advance . parse) $ readFile "input/24.txt"


neighbors2 :: [(Int,Int,Int,Int)]
neighbors2 = [
    (0,  foldl setBit 0 [         1,  5], foldl setBit 0 [11, 7], 0),
    (1,  foldl setBit 0 [     0,  2,  6], foldl setBit 0 [    7], 0),
    (2,  foldl setBit 0 [     1,  3,  7], foldl setBit 0 [    7], 0),
    (3,  foldl setBit 0 [     2,  4,  8], foldl setBit 0 [    7], 0),
    (4,  foldl setBit 0 [     3,      9], foldl setBit 0 [13, 7], 0),

    (5,  foldl setBit 0 [ 0,      6, 10], foldl setBit 0 [11], 0),
    (6,  foldl setBit 0 [ 1,  5,  7, 11], foldl setBit 0 [  ], 0),
    (7,  foldl setBit 0 [ 2,  6,  8    ], foldl setBit 0 [  ], foldl setBit 0 [0,1,2,3,4]),
    (8,  foldl setBit 0 [ 3,  7,  9, 13], foldl setBit 0 [  ], 0),
    (9,  foldl setBit 0 [ 4,  8,     14], foldl setBit 0 [13], 0),

    (10, foldl setBit 0 [ 5,     11, 15], foldl setBit 0 [11], 0),
    (11, foldl setBit 0 [ 6, 10,     16], foldl setBit 0 [  ], foldl setBit 0 [0,5,10,15,20]),
    (12, foldl setBit 0 [              ], foldl setBit 0 [  ], 0),
    (13, foldl setBit 0 [ 8,     14, 18], foldl setBit 0 [  ], foldl setBit 0 [4,9,14,19,24]),
    (14, foldl setBit 0 [ 9, 13,     19], foldl setBit 0 [13 ], 0),

    (15, foldl setBit 0 [10,     16, 20], foldl setBit 0 [11], 0),
    (16, foldl setBit 0 [11, 15, 17, 21], foldl setBit 0 [  ], 0),
    (17, foldl setBit 0 [    16, 18, 22], foldl setBit 0 [  ], foldl setBit 0 [20,21,22,23,24]),
    (18, foldl setBit 0 [13, 17, 19, 23], foldl setBit 0 [  ], 0),
    (19, foldl setBit 0 [14, 18,     24], foldl setBit 0 [13], 0),

    (20, foldl setBit 0 [15,     21    ], foldl setBit 0 [11, 17], 0),
    (21, foldl setBit 0 [16, 20, 22    ], foldl setBit 0 [    17], 0),
    (22, foldl setBit 0 [17, 21, 23    ], foldl setBit 0 [    17], 0),
    (23, foldl setBit 0 [18, 22, 24    ], foldl setBit 0 [    17], 0),
    (24, foldl setBit 0 [19, 23        ], foldl setBit 0 [13, 17], 0)
    ]

advance2 :: [Int] -> [Int]
advance2 grid = dropWhile (== 0) $ map advanceHypertile hypergrid
  where
    hypergrid = zip3 (0:grid ++ [0]) (grid ++ [0,0]) (0:0:grid)
    advanceHypertile (g0,gouter,ginner) = foldr advanceTile g0 neighbors2
      where
        advanceTile (bit,adj0,adjouter,adjinner) g
          | neighborCount == 1 = setBit g bit
          | neighborCount == 2 = complementBit g bit
          | otherwise = clearBit g bit 
          where neighborCount = popCount (adj0 .&. g0) + popCount (adjouter .&. gouter) + popCount (adjinner .&. ginner)

test2 :: ()
test2
  | m5 /= parse "..#..\n.#.#.\n..?.#\n.#.#.\n..#.." = error "m5"
  | m4 /= parse "...#.\n...##\n..?..\n...##\n...#." = error "m4"
  | m3 /= parse "#.#..\n.#...\n..?..\n.#...\n#.#.." = error "m3"
  | m2 /= parse ".#.##\n....#\n..?.#\n...##\n.###." = error "m2"
  | m1 /= parse "#..##\n...##\n..?..\n...#.\n.####" = error "m1"
  | p0 /= parse ".#...\n.#.##\n.#?..\n.....\n....." = error "p0"
  | p1 /= parse ".##..\n#..##\n..?.#\n##.##\n#####" = error "p1"
  | p2 /= parse "###..\n##.#.\n#.?..\n.#.##\n#.#.." = error "p2"
  | p3 /= parse "..###\n.....\n#.?..\n#....\n#...#" = error "p3"
  | p4 /= parse ".###.\n#..#.\n#.?..\n##.#.\n....." = error "p4"
  | p5 /= parse "####.\n#..#.\n#.?#.\n####.\n....." = error "p5"
  | sum (map popCount t10) /= 99 = error "a"
  | otherwise = ()
  where
    t10 = head $ drop 10 $ iterate advance2 [parse $ head testData]
    (p5:p4:p3:p2:p1:p0:m1:m2:m3:m4:m5:_) = t10

part2 :: IO Int
part2 = fmap (sum . map popCount . head . drop 200 . iterate advance2 . (:[]) . parse) $ readFile "input/24.txt"
