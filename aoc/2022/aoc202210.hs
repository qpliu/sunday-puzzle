exec :: Int -> Int -> [String] -> [(Int,(Int,Int))]
exec x cycles ("noop":rest) = exec x (cycles+1) rest
exec x cycles ("addx":n:rest) = (cycles+2,(x,x+read n)):exec (x+read n) (cycles+2) rest
exec _ _ _ = []

xAtT :: [Int] -> [(Int,(Int,Int))] -> [(Int,Int)]
xAtT [] changes = []
xAtT (t:rest) changes@[(changeT,(xbefore,xt))]
  | t < changeT = (t,xbefore) : xAtT rest changes
  | otherwise = (t,xt) : zip rest (repeat xt)
xAtT (t:rest) changes@((changeT,(xbefore,xt)):moreChanges)
  | t < changeT = (t,xbefore) : xAtT rest changes
  | otherwise = xAtT (t:rest) moreChanges

testData :: String
testData = unlines [
    "addx 15",
    "addx -11",
    "addx 6",
    "addx -3",
    "addx 5",
    "addx -1",
    "addx -8",
    "addx 13",
    "addx 4",
    "noop",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx 5",
    "addx -1",
    "addx -35",
    "addx 1",
    "addx 24",
    "addx -19",
    "addx 1",
    "addx 16",
    "addx -11",
    "noop",
    "noop",
    "addx 21",
    "addx -15",
    "noop",
    "noop",
    "addx -3",
    "addx 9",
    "addx 1",
    "addx -3",
    "addx 8",
    "addx 1",
    "addx 5",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx -36",
    "noop",
    "addx 1",
    "addx 7",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "addx 6",
    "noop",
    "noop",
    "noop",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx 7",
    "addx 1",
    "noop",
    "addx -13",
    "addx 13",
    "addx 7",
    "noop",
    "addx 1",
    "addx -33",
    "noop",
    "noop",
    "noop",
    "addx 2",
    "noop",
    "noop",
    "noop",
    "addx 8",
    "noop",
    "addx -1",
    "addx 2",
    "addx 1",
    "noop",
    "addx 17",
    "addx -9",
    "addx 1",
    "addx 1",
    "addx -3",
    "addx 11",
    "noop",
    "noop",
    "addx 1",
    "noop",
    "addx 1",
    "noop",
    "noop",
    "addx -13",
    "addx -19",
    "addx 1",
    "addx 3",
    "addx 26",
    "addx -30",
    "addx 12",
    "addx -1",
    "addx 3",
    "addx 1",
    "noop",
    "noop",
    "noop",
    "addx -9",
    "addx 18",
    "addx 1",
    "addx 2",
    "noop",
    "noop",
    "addx 9",
    "noop",
    "noop",
    "noop",
    "addx -1",
    "addx 2",
    "addx -37",
    "addx 1",
    "addx 3",
    "noop",
    "addx 15",
    "addx -21",
    "addx 22",
    "addx -6",
    "addx 1",
    "noop",
    "addx 2",
    "addx 1",
    "noop",
    "addx -10",
    "noop",
    "noop",
    "addx 20",
    "addx 1",
    "addx 2",
    "addx 2",
    "addx -6",
    "addx -11",
    "noop",
    "noop",
    "noop"
    ]

test :: ()
test
  | (sum . map (uncurry (*)) . xAtT [20,60 .. 220] . exec 1 1 . words) testData /= 13140 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (uncurry (*)) . xAtT [20,60 .. 220] . exec 1 1 . words) $ readFile "input/10.txt"

draw :: String -> String
draw = concatMap pixel . xAtT [1..240] . exec 1 1 . words
  where
    pixel (t,x)
      | abs (x - (t-1) `mod` 40) < 2 = "#" ++ nextRow t
      | otherwise = "." ++ nextRow t
    nextRow t
      | t `mod` 40 == 0 = "\n"
      | otherwise = ""

testResult2 :: String
testResult2 = unlines [
    "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######....."
    ]

test2 :: ()
test2
  | draw testData /= testResult2 = error "a"
  | otherwise = ()

part2 :: IO ()
part2 = do
    input <- readFile "input/10.txt"
    putStr $ draw input
