import Data.Set(Set,fromList,member,partition,size,union)
import qualified Data.Set

parse :: String -> (Set (Int,Int),[(Char,Int)])
parse str = (fromList $ map toXY coords,map getFold (drop 1 folds))
  where
    (coords,folds) = span (/= "") (lines str)
    toXY coord = read ("("++coord++")")
    getFold ('f':'o':'l':'d':' ':'a':'l':'o':'n':'g':' ':axis:'=':n) = (axis,read n)

doFold :: Set (Int,Int) -> (Char,Int) -> Set (Int,Int)
doFold paper ('x',n) = union left (Data.Set.map fold right)
  where
    (left,right) = partition ((< n) . fst) paper
    fold (x,y) = (2*n-x,y)
doFold paper ('y',n) = union top (Data.Set.map fold bottom)
  where
    (top,bottom) = partition ((< n) . snd) paper
    fold (x,y) = (x,2*n-y)

testData :: String
testData = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5\n"

test :: ()
test
  | size (doFold paper (head folds)) /= 17 = error "a"
  | otherwise = ()
  where (paper,folds) = parse testData

part1 :: IO Int
part1 = do
    (paper,folds) <- fmap parse $ readFile "input/13.txt"
    return $ size $ doFold paper (head folds)

doFolds :: (Set (Int,Int),[(Char,Int)]) -> Set (Int,Int)
doFolds (paper,folds) = foldl doFold paper folds

display :: Set (Int,Int) -> String
display paper = unlines [[if member (x,y) paper then '#' else '.' | x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    xmin = minimum $ Data.Set.map fst paper
    xmax = maximum $ Data.Set.map fst paper
    ymin = minimum $ Data.Set.map snd paper
    ymax = maximum $ Data.Set.map snd paper

test2 :: IO ()
test2 = (putStr . display . doFolds . parse) testData

part2 :: IO ()
part2 = readFile "input/13.txt" >>= putStr . display . doFolds . parse
