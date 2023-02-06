import Data.List(nub)
import Data.Map(Map,fromList,keys,member,(!))

parse :: String -> ((Int,Int),Map (Int,Int) Int)
parse = p 0 0 0 0 []
  where
    p x y xmax ymax trees [] = ((xmax,ymax),fromList trees)
    p x y xmax ymax trees (c:rest)
      | c == '\n' = p 0 (y+1) xmax ymax trees rest
      | otherwise = p (x+1) y (max x xmax) (max y ymax) (((x,y),read [c]):trees) rest

visible :: ((Int,Int),Map (Int,Int) Int) -> [(Int,Int)]
visible ((xmax,ymax),trees) = nub $ concatMap verticals [0..xmax] ++ concatMap horizontals [0..ymax]
  where
    verticals x = from (x,0) (-1) (fmap succ) ++ from (x,ymax) (-1) (fmap pred)
    horizontals y = from (0,y) (-1) (\ (x,y) -> (x+1,y)) ++ from (xmax,y) (-1) (\ (x,y) -> (x-1,y))
    from xy hgt next
      | not (member xy trees) = []
      | hgt >= trees!xy = from (next xy) hgt next
      | otherwise = xy : from (next xy) (trees!xy) next

testData :: String
testData = unlines [
    "30373",
    "25512",
    "65332",
    "33549",
    "35390"
    ]

test :: ()
test
  | (length . visible . parse) testData /= 21 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . visible . parse) $ readFile "input/08.txt"

scenicScore :: ((Int,Int),Map (Int,Int) Int) -> (Int,Int) -> Int
scenicScore (_,trees) xy =
    score 0 up (up xy) * score 0 dn (dn xy) * score 0 lt (lt xy) * score 0 rt (rt xy)
  where
    up (x,y) = (x,y-1)
    dn (x,y) = (x,y+1)
    lt (x,y) = (x-1,y)
    rt (x,y) = (x+1,y)
    score dist dir xyscan
      | not (member xyscan trees) = dist
      | trees!xyscan >= trees!xy = dist+1
      | otherwise = score (dist+1) dir (dir xyscan)

bestScenic :: ((Int,Int),Map (Int,Int) Int) -> Int
bestScenic grid@(_,trees) = maximum $ map (scenicScore grid) $ keys trees

test2 :: ()
test2
  | (bestScenic . parse) testData /= 8 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (bestScenic . parse) $ readFile "input/08.txt"
