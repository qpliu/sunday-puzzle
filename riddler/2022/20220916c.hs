import Data.List(sort)
import Data.Set(Set,fromList,member,size,toList)

main :: IO ()
main = do
    words <- (fmap (fromList . map sort . filter ((>= 4) . length) . lines) . readFile) "word.list"
    print (size words)
    print (length $ filter ((== 4) . length) $ toList words)
    scan 0 0 [] 0 0 [] $ concatMap (chains words) $ filter ((== 4) . length) $ toList words

links :: Set String -> String -> [String]
links words letters = [sort (letter:letters) | letter <- ['a'..'z'], member (sort (letter:letters)) words]

chains :: Set String -> String -> [[String]]
chains words letters
  | null nexts && length letters < 9 = []
  | null nexts = [[letters]]
  | otherwise = map (letters:) nexts
  where nexts = concatMap (chains words) (links words letters)

scan :: Int -> Int -> [String] -> Int -> Int -> [String] -> [[String]] -> IO ()
scan totalCount anigCount _ longestCount longestLen _ [] =
    print (totalCount,anigCount,longestCount,longestLen)
scan totalCount anigCount anig longestCount longestLen longest (chain:chains)
  | longestLen > 12 && length chain >= longestLen = do
      print (totalCount,newAnigCount,newLongestCount,newLongestLen,chain)
      next
  | totalCount `mod` 100000 == 0 = do
      print (totalCount,newAnigCount,newLongestCount,newLongestLen)
      next
  | otherwise = next
  where
    next = scan (totalCount+1) newAnigCount newAnig newLongestCount newLongestLen newLongest chains
    (newAnigCount,newAnig)
      | take 6 chain == anig = (anigCount,anig)
      | otherwise = (anigCount+1,take 6 chain)
    (newLongestCount,newLongestLen,newLongest)
      | length chain == longestLen = (longestCount+1,longestLen,chain)
      | length chain > longestLen = (1,length chain,chain)
      | otherwise = (longestCount,longestLen,longest)
