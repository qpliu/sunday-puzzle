move :: (Int,Int) -> [String] -> (Int,Int)
move (x,y) ("forward":n:rest) = move (x+read n,y) rest
move (x,y) ("down":n:rest) = move (x,y+read n) rest
move (x,y) ("up":n:rest) = move (x,y-read n) rest
move (x,y) _ = (x,y)

testData :: String
testData = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2\n"

test :: ()
test
  | (move (0,0) . words) testData /= (15,10) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (uncurry (*) . move (0,0) . words) $ readFile "input/02.txt"

move2 :: ((Int,Int),Int) -> [String] -> ((Int,Int),Int)
move2 ((x,y),aim) ("forward":n:rest) = move2 ((x+read n,y+aim*read n),aim) rest
move2 ((x,y),aim) ("down":n:rest) = move2 ((x,y),aim+read n) rest
move2 ((x,y),aim) ("up":n:rest) = move2 ((x,y),aim-read n) rest
move2 ((x,y),aim) _ = ((x,y),aim)

test2 :: ()
test2
  | (move2 ((0,0),0) . words) testData /= ((15,60),10) = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (uncurry (*) . fst . move2 ((0,0),0) . words) $ readFile "input/02.txt"
