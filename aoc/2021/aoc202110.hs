{-
--- Day 10: Syntax Scoring ---

You ask the submarine to determine the best route out of the deep-sea cave, but
it only replies:

| Syntax error in navigation subsystem on line: all of them

All of them?! The damage is worse than you thought. You bring up a copy of the
navigation subsystem (your puzzle input).

The navigation subsystem syntax is made of several lines containing chunks.
There are one or more chunks on each line, and chunks contain zero or more
other chunks. Adjacent chunks are not separated by any delimiter; if one chunk
stops, the next chunk (if any) can immediately start. Every chunk must open and
close with one of four legal pairs of matching characters:

 - If a chunk opens with (, it must close with ).
 - If a chunk opens with [, it must close with ].
 - If a chunk opens with {, it must close with }.
 - If a chunk opens with <, it must close with >.

So, () is a legal chunk that contains no other chunks, as is []. More complex
but valid chunks include ([]), {()()()}, <([{}])>, [<>({}){}[([])<>]], and
even (((((((((()))))))))).

Some lines are incomplete, but others are corrupted. Find and discard the
corrupted lines first.

A corrupted line is one where a chunk closes with the wrong character - that
is, where the characters it opens and closes with do not form one of the four
legal pairs listed above.

Examples of corrupted chunks include (], {()()()>, (((()))}, and
<([]){()}[{}]). Such a chunk can appear anywhere within a line, and its
presence causes the whole line to be considered corrupted.

For example, consider the following navigation subsystem:

| [({(<(())[]>[[{[]{<()<>>
| [(()[<>])]({[<{<<[]>>(
| {([(<{}[<>[]}>{[]{[(<()>
| (((({<>}<{<{<>}{[]{[]{}
| [[<[([]))<([[{}[[()]]]
| [{[{({}]{}}([{[{{{}}([]
| {<[[]]>}<{[{[{[]{()[[[]
| [<(<(<(<{}))><([]([]()
| <{([([[(<>()){}]>(<<{{
| <{([{{}}[<[[[<>{}]]]>[]]

Some of the lines aren't corrupted, just incomplete; you can ignore these lines
for now. The remaining five lines are corrupted:

 - {([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead.
 - [[<[([]))<([[{}[[()]]] - Expected ], but found ) instead.
 - [{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead.
 - [<(<(<(<{}))><([]([]() - Expected >, but found ) instead.
 - <{([([[(<>()){}]>(<<{{ - Expected ], but found > instead.

Stop at the first incorrect closing character on each corrupted line.

Did you know that syntax checkers actually have contests to see who can get the
high score for syntax errors in a file? It's true! To calculate the syntax
error score for a line, take the first illegal character on the line and look
it up in the following table:

 - ): 3 points.
 - ]: 57 points.
 - }: 1197 points.
 - >: 25137 points.

In the above example, an illegal ) was found twice (2*3 = 6 points), an illegal
] was found once (57 points), an illegal } was found once (1197 points), and an
illegal > was found once (25137 points). So, the total syntax error score for
this file is 6+57+1197+25137 = 26397 points!

Find the first illegal character in each corrupted line of the navigation
subsystem. What is the total syntax error score for those errors?
-}

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
