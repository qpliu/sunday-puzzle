import Data.List(intersperse,maximumBy,subsequences)
import Data.Map(Map,(!))
import qualified Data.Map
import Data.Set(Set,delete,difference,elems,findMin)
import qualified Data.Set

states :: Int -> Int -> [[Set Int]]
states npoles ndiscs = construct npoles (Data.Set.fromList [1..ndiscs])
  where
    construct :: Int -> Set Int -> [[Set Int]]
    construct n items
      | n <= 1 = [[items]]
      | otherwise = concatMap cons (map Data.Set.fromList (subsequences (elems items)))
      where
        cons :: Set Int -> [[Set Int]]
        cons first = map (first:) (construct (n-1) (difference items first))

moves :: [Set Int] -> Set [Set Int]
moves start = Data.Set.fromList $ filter (/= start) $ concatMap add1 $ concatMap remove1 start
  where
    remove1 :: Set Int -> [Int]
    remove1 pole | Data.Set.null pole = [] | otherwise = [findMin pole]
    add1 :: Int -> [[Set Int]]
    add1 disc = tryAdd1 disc [] $ map (delete disc) start
    tryAdd1 :: Int -> [Set Int] -> [Set Int] -> [[Set Int]]
    tryAdd1 disc previousPoles [] = []
    tryAdd1 disc previousPoles (pole:poles)
      | Data.Set.null pole || findMin pole > disc = (reverse previousPoles ++ (Data.Set.insert disc pole:poles)) : tryAdd1 disc (pole:previousPoles) poles
      | otherwise = tryAdd1 disc (pole:previousPoles) poles

isStart :: [Set Int] -> Bool
isStart (startPole:poles) = all Data.Set.null poles

isLast :: [Set Int] -> Bool
isLast (startPole:onePole:poles) = all Data.Set.null poles && Data.Set.size onePole == 1 && maximum onePole < minimum startPole

isEnd :: [Set Int] -> Bool
isEnd (startPole:poles) = Data.Set.null startPole && length (filter (not . Data.Set.null) poles) == 1

isParticularEnd :: [Set Int] -> Bool
isParticularEnd (startPole:poles) = all Data.Set.null (startPole:drop 1 poles)

renderEquation :: ([Set Int] -> [Set Int]) -> [Set Int] -> String
renderEquation canon state
  | isEnd state = name state ++ " = 0"
  | otherwise = name state ++ concatMap (nameNext (Data.Set.size nexts)) nexts ++ " = 1"
  where
    nexts = moves state
    nameNext n s = " - " ++ name s ++ "/" ++ show n
    name s = "m_{" ++ concat (intersperse "," (map (concatMap show . elems) $ canon s)) ++ "}"

canon3 :: [Set Int] -> [Set Int]
canon3 [a,b,c] = [a,max b c,min b c]

isCanon3 :: [Set Int] -> Bool
isCanon3 s = s == canon3 s

equation :: Map [Set Int] Int -> [Set Int] -> ([Rational],Rational)
equation table state = (map getCoeff [1 .. Data.Map.size table],1)
  where
    -- table should not include end states
    nexts = moves state
    thisi = table!state
    nextis = concatMap (maybe [] (:[]) . flip Data.Map.lookup table) nexts
    nextCoeff = -1 / fromIntegral (length nexts)
    getCoeff i | i == thisi = 1
               | i `elem` nextis = nextCoeff
               | otherwise = 0

equations :: ([Set Int] -> Bool) -> ([Set Int] -> Bool) -> [[Set Int]] -> [([Rational],Rational)]
equations isStart isEnd states = map (equation (Data.Map.fromList (zip nonEndStates [1..]))) nonEndStates
  where
    -- make sure the start state is at the end
    nonEndStates = filter (not . isStart) (filter (not . isEnd) states) ++ filter isStart states

upperTriangularize :: [([Rational],Rational)] -> [([Rational],Rational)]
upperTriangularize eqns = ut 0 eqns
  where
    n = length $ fst $ head eqns
    ut i eqs
      | i >= n = eqs
      | otherwise = ut (i+1) (map subPivot eqs)
      where
        subPivot row@(coefs,rhs)
          | row == pivotRow = row
          | otherwise = (zipWith (-) coefs (map (*factor) pivotRowCoefs),rhs - factor*pivotRowRhs)
          where
            factor = head (drop i coefs) / pivotCoef
        pivotCoef = head (drop i pivotRowCoefs)
        pivotRow@(pivotRowCoefs,pivotRowRhs) = maximumBy compareForPivot (filter (all (== 0) . take i . fst) eqs)
        compareForPivot (coefs1,_) (coefs2,_) = compare (abs (head (drop i coefs1))) (abs (head (drop i coefs2)))

findAnswer :: [([Rational],Rational)] -> Rational
findAnswer eqns = rhs / last coefs
  where
    n = length eqns
    ((coefs,rhs):_) = filter (all (== 0) . take (n-1) . fst) eqns

main :: IO ()
main = do
    putStrLn "3-disc equations:"
    mapM_ putStrLn (map (renderEquation id) (states 3 3))
    putStrLn "3-disc equations removing symmetry:"
    mapM_ putStrLn (map (renderEquation canon3) (filter isCanon3 (states 3 3)))
    putStrLn "4-disc equations removing symmetry:"
    mapM_ putStrLn (map (renderEquation canon3) (filter isCanon3 (states 3 4)))
    putStr "2-disc solution: "
    print (findAnswer $ upperTriangularize $ equations isStart isEnd (states 3 2))
    putStr "3-disc solution: "
    print (findAnswer $ upperTriangularize $ equations isStart isEnd (states 3 3))
    putStr "4-disc solution: "
    print (findAnswer $ upperTriangularize $ equations isStart isEnd (states 3 4))
    putStr "3-disc and 4 poles solution: "
    print (findAnswer $ upperTriangularize $ equations isStart isEnd (states 4 3))
    putStr "3-disc solution for particular target pole: "
    print (findAnswer $ upperTriangularize $ equations isStart isParticularEnd (states 3 3))
    putStr "4-disc solution for particular target pole: "
    print (findAnswer $ upperTriangularize $ equations isStart isParticularEnd (states 3 4))
    putStr "3-disc and 4 poles solution for particular target pole: "
    print (findAnswer $ upperTriangularize $ equations isStart isParticularEnd (states 4 3))
    putStr "5-disc solution: "
    print (findAnswer $ upperTriangularize $ equations isStart isEnd (states 3 5))
    putStr "m_last,2: "
    print (findAnswer $ upperTriangularize $ equations isLast isStart (states 3 2))
    putStr "m_last,3: "
    print (findAnswer $ upperTriangularize $ equations isLast isStart (states 3 3))
    putStr "m_last,4: "
    print (findAnswer $ upperTriangularize $ equations isLast isStart (states 3 4))
