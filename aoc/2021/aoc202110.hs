import Data.List(sort)

classify :: [String] -> ([(String,Char)],([(String,String)],[String]))
classify = foldr parse ([],([],[]))
  where
    parse str (corrupt,(incomplete,valid)) = p [] str
      where
        p stack ""
          | null stack = (corrupt,(incomplete,str:valid))
          | otherwise = (corrupt,((str,stack):incomplete,valid))
        p stack ('(':rest) = p (')':stack) rest
        p stack ('[':rest) = p (']':stack) rest
        p stack ('{':rest) = p ('}':stack) rest
        p stack ('<':rest) = p ('>':stack) rest
        p stack (c:rest)
          | take 1 stack == [c] = p (drop 1 stack) rest
          | otherwise = ((str,c):corrupt,(incomplete,valid))

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score c = error (show c)

testData :: String
testData = "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]\n"

test :: ()
test
  | (sum . map (score . snd) . fst . classify . lines) testData /= 26397 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (sum . map (score . snd) . fst . classify . lines) $ readFile "input/10.txt"

score2 :: Int -> String -> Int
score2 acc "" = acc
score2 acc (ch:rest)
  | ch == ')' = score2 (1+5*acc) rest
  | ch == ']' = score2 (2+5*acc) rest
  | ch == '}' = score2 (3+5*acc) rest
  | ch == '>' = score2 (4+5*acc) rest
  | otherwise = error (ch:rest)

middle :: [Int] -> Int
middle scores = head $ drop (length scores `div` 2) $ sort scores

test2 :: ()
test2
  | (map (score2 0 . snd) . fst . snd . classify . lines) testData /= [288957,5566,1480781,995444,294] = error "a"
  | (middle . map (score2 0 . snd) . fst . snd . classify . lines) testData /= 288957 = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (middle . map (score2 0 . snd) . fst . snd . classify . lines) $ readFile "input/10.txt"
