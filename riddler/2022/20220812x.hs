import Data.Set(Set,difference,fromList,insert,isSubsetOf,member,partition,size,toList)
import qualified Data.Set

smap :: Ord b => (a -> b) -> Set a -> Set b
smap = Data.Set.map

canonical :: Set (Int,Int) -> Set (Int,Int)
canonical set = minimum (map recenter (take 4 (iterate (smap rotate) set) ++ take 4 (iterate (smap rotate) (smap reflect set))))
  where
    recenter set = smap (translate (minimum (smap fst set),minimum (smap snd set))) set
    translate (x0,y0) (x,y) = (x-x0,y-y0)
    rotate (x,y) = (y,-x)
    reflect (x,y) = (x,-y)

addSquare :: Set (Int,Int) -> [Set (Int,Int)]
addSquare set = toList $ fromList $ map canonical [insert (x,y) set | x <- [minimum (smap fst set) - 1 .. maximum (smap fst set) + 1], y <- [minimum (smap snd set) - 1 .. maximum (smap snd set) + 1], not ((x,y) `member` set), (x-1,y) `member` set || (x+1,y) `member` set ||  (x,y-1) `member` set ||  (x,y+1) `member` set]

addSquares :: [Set (Int,Int)] -> [Set (Int,Int)]
addSquares sets = toList $ fromList $ concatMap addSquare sets

xrange :: Set (Int,Int) -> [Int]
xrange set = [minimum (smap fst set) .. maximum (smap fst set)]

yrange :: Set (Int,Int) -> [Int]
yrange set = [minimum (smap snd set) .. maximum (smap snd set)]

showXomino :: Set (Int,Int) -> String
showXomino set = unlines [[if (x,y) `member` set then '#' else ' ' | y <- yrange set] | x <- xrange set]

decomposeable :: Int -> Set (Int,Int) -> Bool
decomposeable n set
  | n == 0 = True
  | n == 1 = size set > 0
  | otherwise = or [decomposeable (n-1) (difference set array) | x <- xrange set, y <- yrange set, array <- [fromList [(x+i,y) | i <- [0..n-1]],fromList [(x,y+i) | i <- [0..n-1]]], array `isSubsetOf` set]

main :: IO ()
main = do
    mapM_ (\ s -> print (decomposeable 3 s) >> putStr (showXomino s)) $ head $ drop 5 $ iterate addSquares [fromList [(0,0)]]
    (\ (a,b) -> print (size b,size a+size b)) $ partition (decomposeable 2) $ fromList $ head $ drop 2 $ iterate addSquares [fromList [(0,0)]]
    (\ (a,b) -> print (size b,size a+size b)) $ partition (decomposeable 3) $ fromList $ head $ drop 5 $ iterate addSquares [fromList [(0,0)]]
    (\ (a,b) -> print (size b,size a+size b)) $ partition (decomposeable 4) $ fromList $ head $ drop 9 $ iterate addSquares [fromList [(0,0)]]
