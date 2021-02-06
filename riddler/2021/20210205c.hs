import Data.List(intersperse,subsequences)
import Data.Set(Set,delete,difference,elems,findMin,fromList,size)
import qualified Data.Set

states :: Int -> Int -> [[Set Int]]
states npoles ndiscs = construct npoles (fromList [1..ndiscs])
  where
    construct :: Int -> Set Int -> [[Set Int]]
    construct n items
      | n <= 1 = [[items]]
      | otherwise = concatMap cons (map fromList (subsequences (elems items)))
      where
        cons :: Set Int -> [[Set Int]]
        cons first = map (first:) (construct (n-1) (difference items first))

moves :: [Set Int] -> Set [Set Int]
moves start = fromList $ filter (/= start) $ concatMap add1 $ concatMap remove1 start
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

isEnd :: [Set Int] -> Bool
isEnd (startPole:poles) = Data.Set.null startPole && length (filter (not . Data.Set.null) poles) == 1

equation :: ([Set Int] -> [Set Int]) -> [Set Int] -> String
equation canon state
  | isEnd state = name state ++ " = 0"
  | otherwise = name state ++ concatMap (nameNext (size nexts)) nexts ++ " = 1"
  where
    nexts = moves state
    nameNext n s = " - " ++ name s ++ "/" ++ show n
    name s = "m_{" ++ concat (intersperse "," (map (concatMap show . elems) $ canon s)) ++ "}"

canon3 :: [Set Int] -> [Set Int]
canon3 [a,b,c] = [a,max b c,min b c]

isCanon3 :: [Set Int] -> Bool
isCanon3 s = s == canon3 s

main :: IO ()
main = do
    putStrLn "3-disc equations:"
    mapM_ putStrLn (map (equation id) (states 3 3))
    putStrLn "3-disc equations removing symmetry:"
    mapM_ putStrLn (map (equation canon3) (filter isCanon3 (states 3 3)))
    putStrLn "4-disc equations removing symmetry:"
    mapM_ putStrLn (map (equation canon3) (filter isCanon3 (states 3 4)))
    putStrLn "5-disc equations removing symmetry:"
    mapM_ putStrLn (map (equation canon3) (filter isCanon3 (states 3 5)))
