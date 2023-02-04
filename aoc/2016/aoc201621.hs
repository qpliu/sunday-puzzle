-- Note that abcdefgh does not have any repeated letters.

scramble :: String -> [String] -> String
scramble pwd ("swap":"position":x:"with":"position":y:_) = swapPosition pwd (read x) (read y)
scramble pwd ("swap":"letter":x:"with":"letter":y:_) = swapLetter pwd (head x) (head y)
scramble pwd ("rotate":"left":x:_) = rotateLeft pwd (read x)
scramble pwd ("rotate":"right":x:_) = rotateRight pwd (read x)
scramble pwd ("rotate":"based":"on":"position":"of":"letter":x:_) = rotateBasedOn pwd (head x)
scramble pwd ("reverse":"positions":x:"through":y:_) = reversePositions pwd (read x) (read y)
scramble pwd ("move":"position":x:"to":"position":y:_) = movePosition pwd (read x) (read y)
scramble pwd _ = pwd

swapPosition :: String -> Int -> Int -> String
swapPosition pwd x y = take i pwd ++ take 1 (drop j pwd) ++ drop (i+1) (take j pwd) ++ take 1 (drop i pwd) ++ drop (j+1) pwd
  where
    i = min x y
    j = max x y

swapLetter :: String -> Char -> Char -> String
swapLetter "" _ _ = ""
swapLetter (c:cs) x y
  | x == c = y : swapLetter cs x y
  | y == c = x : swapLetter cs x y
  | otherwise = c : swapLetter cs x y

rotateLeft :: String -> Int -> String
rotateLeft pwd x = drop rot pwd ++ take rot pwd
  where rot = x `mod` length pwd

rotateRight :: String -> Int -> String
rotateRight pwd x = drop rot pwd ++ take rot pwd
  where rot = length pwd - (x `mod` length pwd)

rotateBasedOn :: String -> Char -> String
rotateBasedOn pwd x = rotateRight pwd (position + if position >= 4 then 2 else 1)
  where
    Just position = lookup x $ zip pwd [0..]

reversePositions :: String -> Int -> Int -> String
reversePositions pwd x y = take x pwd ++ reverse (drop x $ take (y+1) pwd) ++ drop (y+1) pwd

movePosition :: String -> Int -> Int -> String
movePosition pwd x y
  | x == y = pwd
  | x < y = take x pwd ++ drop (x+1) (take (y+1) pwd) ++ take 1 (drop x pwd) ++ drop (y+1) pwd
  | otherwise = take y pwd ++ take 1 (drop x pwd) ++ drop y (take x pwd) ++ drop (x+1) pwd

test :: ()
test
  | foldl scramble "abcde" (map words $ lines testData) /= "decab" = error "a"
  | otherwise = ()
  where
    testData = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d"

part1 :: IO String
part1 = fmap (foldl scramble "abcdefgh" . map words . lines) $ readFile "input/21.txt"

unscramble :: String -> [String] -> String
unscramble pwd ("swap":"position":x:"with":"position":y:_) = swapPosition pwd (read x) (read y)
unscramble pwd ("swap":"letter":x:"with":"letter":y:_) = swapLetter pwd (head x) (head y)
unscramble pwd ("rotate":"left":x:_) = rotateRight pwd (read x)
unscramble pwd ("rotate":"right":x:_) = rotateLeft pwd (read x)
unscramble pwd ("rotate":"based":"on":"position":"of":"letter":x:_) = unRotateBasedOn pwd (head x)
unscramble pwd ("reverse":"positions":x:"through":y:_) = reversePositions pwd (read x) (read y)
unscramble pwd ("move":"position":x:"to":"position":y:_) = movePosition pwd (read y) (read x)
unscramble pwd _ = pwd

unRotateBasedOn :: String -> Char -> String
unRotateBasedOn pwd x = rotateLeft pwd (findCount 1)
  where
    Just position = lookup x $ zip pwd [0..]
    len = length pwd
    findCount n
      | (position - n) `mod` len == n-1 && n-1 < 4 = n
      | (position - n) `mod` len == n-2 && n-2 >= 4 = n
      | n > len+2 = error "?"
      | otherwise = findCount (n+1)


test2 :: ()
test2
   -- note that "rotate based on position of letter e" applied to both "abcde" and "cdeab" result in "eabcd", so this test fails
  --  | foldl unscramble "decab" (reverse $ map words $ lines testData) /= "abcde" = error "a"
  | otherwise = ()
  where
    testData = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d"

part2 :: IO String
part2 = fmap (foldl unscramble "fbgdceah" . reverse . map words . lines) $ readFile "input/21.txt"
