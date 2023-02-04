import Data.Map(Map,findWithDefault,fromList,keys,mapWithKey,member,(!))

parse :: String -> Map (Int,Int) Int
parse = fromList . p 0 0
  where
    p x y [] = []
    p x y ('\n':rest) = p 0 (y+1) rest
    p x y ('L':rest) = ((x,y),0) : p (x+1) y rest
    p x y ('#':rest) = ((x,y),1) : p (x+1) y rest
    p x y (_:rest) = p (x+1) y rest

step :: Map (Int,Int) Int -> Map (Int,Int) Int
step seats = mapWithKey update seats
  where
    update (x,y) state
      | neighbors == 0 = 1
      | neighbors >= 4 = 0
      | otherwise = state
      where
        neighbors = sum $ [findWithDefault 0 (x+dx,y+dy) seats | dx <- [-1,0,1], dy <- [-1,0,1], dx /= 0 || dy /= 0]

fixed :: Eq a => (a -> a) -> a -> a
fixed f input
  | input == output = input
  | otherwise = fixed f output
  where output = f input

testData :: [String]
testData = [
    "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL\n",
    "#.##.##.##\n#######.##\n#.#.#..#..\n####.##.##\n#.##.##.##\n#.#####.##\n..#.#.....\n##########\n#.######.#\n#.#####.##\n",
    "#.LL.L#.##\n#LLLLLL.L#\nL.L.L..L..\n#LLL.LL.L#\n#.LL.LL.LL\n#.LLLL#.##\n..L.L.....\n#LLLLLLLL#\n#.LLLLLL.L\n#.#LLLL.##\n",
    "#.##.L#.##\n#L###LL.L#\nL.#.#..#..\n#L##.##.L#\n#.##.LL.LL\n#.###L#.##\n..#.#.....\n#L######L#\n#.LL###L.L\n#.#L###.##\n",
    "#.#L.L#.##\n#LLL#LL.L#\nL.L.L..#..\n#LLL.##.L#\n#.LL.LL.LL\n#.LL#L#.##\n..L.L.....\n#L#LLLL#L#\n#.LLLLLL.L\n#.#L#L#.##\n",
    "#.#L.L#.##\n#LLL#LL.L#\nL.#.L..#..\n#L##.##.L#\n#.#L.LL.LL\n#.#L#L#.##\n..L.L.....\n#L#L##L#L#\n#.LLLLLL.L\n#.#L#L#.##\n"
    ]

test :: ()
test
  | map parse testData /= (take 6 . iterate step . parse) (head testData) = error "a"
  | parse (testData !! 5) /= (fixed step . parse) (head testData) = error "b"
  | (sum . fixed step . parse) (head testData) /= 37 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . fixed step . parse) $ readFile "input/11.txt"

makeNeighbors :: Map (Int,Int) Int -> Map (Int,Int) [(Int,Int)]
makeNeighbors seats = mapWithKey neighbors seats
  where
    (xmax,ymax) = (maximum $ map fst $ keys seats,maximum $ map snd $ keys seats)
    neighbors (x,y) _ = concatMap (take 1 . filter (`member` seats)) [
        [(x+i,y) | i <- [1..xmax-x]],
        [(x-i,y) | i <- [1..x]],
        [(x,y+i) | i <- [1..ymax-y]],
        [(x,y-i) | i <- [1..y]],
        [(x+i,y+i) | i <- [1..xmax-x]],
        [(x+i,y-i) | i <- [1..xmax-x]],
        [(x-i,y+i) | i <- [1..x]],
        [(x-i,y-i) | i <- [1..x]]
        ]

step2 :: Map (Int,Int) [(Int,Int)] -> Map (Int,Int) Int -> Map (Int,Int) Int
step2 neighbors seats = mapWithKey update seats
  where
    update xy state
      | n == 0 = 1
      | n >= 5 = 0
      | otherwise = state
      where
        n = sum [findWithDefault 0 nxy seats | nxy <- neighbors!xy]

test2 :: ()
test2
  | (sum . fixed (step2 neighbors)) seats /= 26 = error "a"
  | otherwise = ()
  where
    seats = parse $ head testData
    neighbors = makeNeighbors seats

part2 :: IO Int
part2 = do
    seats <- fmap parse $ readFile "input/11.txt"
    return $ sum $ fixed (step2 (makeNeighbors seats)) seats
