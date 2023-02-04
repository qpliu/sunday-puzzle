parse :: String -> ([Int],[Int])
parse s = (map read $ words s,[])

jumps :: Int -> ([Int],[Int]) -> Int
jumps nsteps ([],_) = nsteps
jumps nsteps (offset:forward,backward)
  | offset >= 0 = jumps (nsteps+1) (newForward,reverse skipForward ++ backward)
  | -offset > length backward = nsteps+1
  | otherwise = jumps (nsteps+1) (reverse skipBackward ++ offset+1:forward,newBackward)
  where (skipForward,newForward) = splitAt offset (offset+1:forward)
        (skipBackward,newBackward) = splitAt (-offset) backward

test :: ()
test
  | jumps 0 ([0,3,0,1,-3],[]) /= 5 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (jumps 0 . parse) $ readFile "input/05.txt"

-- fails with stack overflow
jumps2 :: Int -> ([Int],[Int]) -> Int
jumps2 nsteps ([],_) = nsteps
jumps2 nsteps (offset:forward,backward)
  | offset >= 0 = jumps2 (nsteps+1) (newForward,reverse skipForward ++ backward)
  | -offset > length backward = nsteps+1
  | otherwise = jumps2 (nsteps+1) (reverse skipBackward ++ newOffset:forward,newBackward)
  where (skipForward,newForward) = splitAt offset (newOffset:forward)
        (skipBackward,newBackward) = splitAt (-offset) backward
        newOffset | offset >= 3 = offset - 1 | otherwise = offset + 1

test2 :: ()
test2
  | jumps2 0 ([0,3,0,1,-3],[]) /= 10 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (jumps2 0 . parse) $ readFile "input/05.txt"
