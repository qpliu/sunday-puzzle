skipGarbage :: String -> String
skipGarbage s
  | null s = s
  | take 1 s == ">" = drop 1 s
  | take 1 s == "!" = skipGarbage (drop 2 s)
  | otherwise = skipGarbage (drop 1 s)

score :: Int -> Int -> String -> Int
score runningTotal nesting s
  | null s = runningTotal
  | take 1 s == "<" = score runningTotal nesting $ skipGarbage s
  | take 1 s == "{" = score (runningTotal+nesting+1) (nesting+1) (drop 1 s)
  | take 1 s == "}" = score runningTotal (nesting-1) (drop 1 s)
  | otherwise = score runningTotal nesting (drop 1 s)

test :: ()
test
  | skipGarbage "<>" /= "" = error "a"
  | skipGarbage "<random characters>" /= "" = error "b"
  | skipGarbage "<<<<>" /= "" = error "c"
  | skipGarbage "<{!>}>" /= "" = error "d"
  | skipGarbage "<!!>" /= "" = error "e"
  | skipGarbage "<!!!>>" /= "" = error "e"
  | skipGarbage "<{o\"i!a,<{i<a>" /= "" = error "g"
  | score 0 0 "{}" /= 1 = error "h"
  | score 0 0 "{{{}}}" /= 6 = error "i"
  | score 0 0 "{{},{}}" /= 5 = error "j"
  | score 0 0 "{{{},{},{{}}}}" /= 16 = error "k"
  | score 0 0 "{<a>,<a>,<a>,<a>}" /= 1 = error "l"
  | score 0 0 "{{<ab>},{<ab>},{<ab>},{<ab>}}" /= 9 = error "m"
  | score 0 0 "{{<!!>},{<!!>},{<!!>},{<!!>}}" /= 9 = error "n"
  | score 0 0 "{{<a!>},{<a!>},{<a!>},{<ab>}}" /= 3 = error "o"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (score 0 0) $ readFile "input/09.txt"

skipGarbage2 :: (Int,String) -> (Int,String)
skipGarbage2 (count,s)
  | null s = (count,s)
  | take 1 s == ">" = (count,drop 1 s)
  | take 1 s == "!" = skipGarbage2 (count,drop 2 s)
  | otherwise = skipGarbage2 (count+1,drop 1 s)

count2 :: (Int,String) -> Int
count2 (count,s)
  | null s = count
  | otherwise = count2 $ skipGarbage2 (count,drop 1 $ dropWhile (/= '<') s)

test2 :: ()
test2
  | count2 (0,"<>") /= 0 = error "a"
  | count2 (0,"<random characters>") /= 17 = error "b"
  | count2 (0,"<<<<>") /= 3 = error "c"
  | count2 (0,"<{!>}>") /= 2 = error "d"
  | count2 (0,"<!!>") /= 0 = error "e"
  | count2 (0,"<!!!>>") /= 0 = error "f"
  | count2 (0,"<{o\"i!a,<{i<a>") /= 10 = error "g"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (count2 . ((,) 0)) $ readFile "input/09.txt"
