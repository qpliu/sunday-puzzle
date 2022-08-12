import Data.Set(Set,fromList,insert,member,toList)
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

showXomino :: Set (Int,Int) -> String
showXomino set = unlines [[if (x,y) `member` set then '#' else ' ' | x <- [minimum (smap fst set) .. maximum (smap fst set)]] | y <- [minimum (smap snd set) .. maximum (smap snd set)]]

main :: IO ()
main = mapM_ (\ (i,s) -> print i >> putStr s) $ zip [1..] $ map showXomino $ head $ drop 5 $ iterate addSquares [fromList [(0,0)]]
