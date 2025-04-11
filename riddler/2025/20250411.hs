import Data.Set(Set,empty,fromList,insert,member,size)

seqs :: Set String -> String -> Int -> [(Int,String)]
seqs seen s@(a1:a2:_) len
  | null continue = [(len,reverse s)]
  | otherwise = concat continue
  where
    continue = [seqs (insert [a0,a1,a2] seen) (a0:s) (len+1)
                | a0 <- "ROY", a0 /= a1, not (member [a0,a1,a2] seen)]

main :: IO ()
main = do
    mapM_ (putStrLn . snd) $ filter ((== 14) . fst) $ seqs empty "OR" 2
    putStrLn $ head $ seqs4 empty "ORO" 3
    putStrLn $ head $ seqs4 empty "ORY" 3

seqs4 :: Set String -> String -> Int -> [String]
seqs4 seen s@(a1:a2:a3:_) len
  | len == 99 = [s]
  | null continue = []
  | otherwise = concat continue
  where
    continue = [seqs4 (insert [a0,a1,a2,a3] seen) (a0:s) (len+1)
                | a0 <- "ROYP",
                  a0 /= a1,
                  not (member [a0,a1,a2,a3] seen),
                  size (fromList [a0,a1,a2,a3]) >= 3]
