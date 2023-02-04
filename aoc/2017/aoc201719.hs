import Data.Map(Map,fromList,findWithDefault)

parse :: String -> ((Int,Int),Map (Int,Int) Char)
parse s = (fst $ minimum $ filter ((/= ' ') . snd) list,fromList list)
  where
    list = concat $ zipWith parseRow [0..] (lines s)
    parseRow row line = zip (map ((,) row) [0..]) line

follow :: Map (Int,Int) Char -> (Int,Int) -> (Int,Int) -> String
follow diagram (y,x) (dy,dx)
  | (dy,dx) == (0,0) = ""
  | c (y,x) `elem` "|-+" = follow diagram (newy,newx) (newdy,newdx)
  | otherwise = c (y,x) : follow diagram (newy,newx) (newdy,newdx)
  where
    c pos = findWithDefault ' ' pos diagram
    (newy,newx) = (y+newdy,x+newdx)
    (newdy,newdx)
      | c (y+dy,x+dx) /= ' ' = (dy,dx)
      | c (y+dx,x+dy) /= ' ' = (dx,dy)
      | c (y-dx,x-dy) /= ' ' = (-dx,-dy)
      | otherwise = (0,0)

letters :: String -> String
letters s = follow diagram start (1,0)
  where (start,diagram) = parse s

testData :: String
testData = "     |          \n     |  +--+    \n     A  |  C    \n F---|----E|--+ \n     |  |  |  D \n     +B-+  +--+ "

test :: ()
test
  | letters testData /= "ABCDEF" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap letters $ readFile "input/19.txt"

follow2 :: Map (Int,Int) Char -> (Int,Int) -> (Int,Int) -> Int -> Int
follow2 diagram (y,x) (dy,dx) count
  | (dy,dx) == (0,0) = count
  | otherwise = follow2 diagram (newy,newx) (newdy,newdx) (count+1)
  where
    c pos = findWithDefault ' ' pos diagram
    (newy,newx) = (y+newdy,x+newdx)
    (newdy,newdx)
      | c (y+dy,x+dx) /= ' ' = (dy,dx)
      | c (y+dx,x+dy) /= ' ' = (dx,dy)
      | c (y-dx,x-dy) /= ' ' = (-dx,-dy)
      | otherwise = (0,0)

countSteps :: String -> Int
countSteps s = follow2 diagram start (1,0) 0
  where (start,diagram) = parse s

test2 :: ()
test2
  | countSteps testData /= 38 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap countSteps $ readFile "input/19.txt"
