gen :: String -> String
gen s = gen1 ('.':s++".")
  where
    gen1 (a:s@(b:c:_))
      | [a,b,c] `elem` ["^^.",".^^","^..","..^"] = '^':gen1 s
      | otherwise = '.':gen1 s
    gen1 _ = ""


test :: ()
test
  | take 3 (iterate gen "..^^.") /= ["..^^.",".^^^^","^^..^"] = error "a"
  | take 10 (iterate gen ".^^.^.^^^^") /= [".^^.^.^^^^","^^^...^..^","^.^^.^.^^.","..^^...^^^",".^^^^.^^.^","^^..^.^^..","^^^^..^^^.","^..^^^^.^^",".^^^..^.^^","^^.^^^..^^"] = error "b"
  | length (filter (/= '^') $ concat $ take 10 $ iterate gen ".^^.^.^^^^") /= 38 = error "c"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter (/= '^') . concat . take 40 . iterate gen . head . lines) $ readFile "input/18.txt"

countSafe :: Int -> Int -> String -> Int
countSafe remainingRows runningTotal row
  | remainingRows <= 0 = runningTotal
  | otherwise = countSafe (remainingRows - 1) (runningTotal + length (filter (/= '^') row)) (gen row)

part2 :: IO Int
part2 = fmap (countSafe 400000 0 . head . lines) $ readFile "input/18.txt"
