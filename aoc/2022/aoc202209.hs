import Data.List(nub)

parse :: String -> [(Char,Int)]
parse = p . words
  where
    p ([c]:n:rest) = (c,read n) : p rest
    p _ = []

move :: ((Int,Int),(Int,Int)) -> [(Char,Int)] -> [((Int,Int),(Int,Int))]
move ((xh,yh),(xt,yt)) [] = [((xh,yh),(xt,yt))]
move ((xh,yh),(xt,yt)) ((dir,n):rest)
  | n == 0 = move ((xh,yh),(xt,yt)) rest
  | otherwise = ((xh,yh),(xt,yt)) : move ((newXH,newYH),(newXT,newYT)) ((dir,n-1):rest)
  where
    newXH | dir == 'L' = xh-1 | dir == 'R' = xh+1 | otherwise = xh
    newYH | dir == 'U' = yh-1 | dir == 'D' = yh+1 | otherwise = yh
    newXT
      | newXH > xt + 1 || (newXH > xt && abs (newYH - yt) > 1) = xt + 1
      | newXH < xt - 1 || (newXH < xt && abs (newYH - yt) > 1) = xt - 1
      | otherwise = xt
    newYT
      | newYH > yt + 1 || (newYH > yt && abs (newXH - xt) > 1) = yt + 1
      | newYH < yt - 1 || (newYH < yt && abs (newXH - xt) > 1) = yt - 1
      | otherwise = yt

testData :: String
testData = unlines [
    "R 4",
    "U 4",
    "L 3",
    "D 1",
    "R 4",
    "D 1",
    "L 5",
    "R 2"
    ]

test :: ()
test
  | (length . nub . map snd . move ((0,0),(0,0)) . parse) testData /= 13 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . nub . map snd . move ((0,0),(0,0)) . parse) $ readFile "input/09.txt"

step2 :: [(Int,Int)] -> Char -> [(Int,Int)]
step2 ((xh,yh):t) dir
  | dir == 'L' = moveHeadTo (xh-1,yh) t
  | dir == 'R' = moveHeadTo (xh+1,yh) t
  | dir == 'U' = moveHeadTo (xh,yh-1) t
  | dir == 'D' = moveHeadTo (xh,yh+1) t
  where
    moveHeadTo hxy [] = [hxy]
    moveHeadTo hxy@(hx,hy) t@((tx,ty):trest)
      | abs dx < 2 && abs dy < 2 = hxy:t
      | abs dx == abs dy = hxy : moveHeadTo (tx+dx-signum dx,ty+dy-signum dy) trest
      | abs dx > abs dy = hxy : moveHeadTo (tx+dx-signum dx,ty+dy) trest
      | otherwise = hxy : moveHeadTo (tx+dx,ty+dy-signum dy) trest
      where (dx,dy) = (hx-tx,hy-ty)

move2 :: Int -> [(Int,Int)] -> [(Char,Int)] -> [(Int,Int)]
move2 tracked rope [] = [rope !! tracked]
move2 tracked rope ((dir,n):rest)
  | n <= 0 = move2 tracked rope rest
  | otherwise = (rope !! tracked) : move2 tracked (step2 rope dir) ((dir,n-1):rest)

testData2 :: String
testData2 = unlines [
    "R 5",
    "U 8",
    "L 8",
    "D 3",
    "R 17",
    "D 10",
    "L 25",
    "U 20"
    ]

test2 :: ()
test2
  | (length . nub . move2 1 (take 2 $ repeat (0,0)) . parse) testData /= 13 = error "a"
  | (length . nub . move2 9 (take 10 $ repeat (0,0)) . parse) testData /= 1 = error "b"
  | (length . nub . move2 9 (take 10 $ repeat (0,0)) . parse) testData2 /= 36 = error "c"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . nub . move2 9 (take 10 $ repeat (0,0)) . parse) $ readFile "input/09.txt"
