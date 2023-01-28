{-
--- Day 19: Monster Messages ---

You land in an airport surrounded by dense forest. As you walk to your
high-speed train, the Elves at the Mythical Information Bureau contact you
again. They think their satellite has collected an image of a sea monster!
Unfortunately, the connection to the satellite is having problems, and many of
the messages sent back from the satellite have been corrupted.

They sent you a list of the rules valid messages should obey and a list of
received messages they've collected so far (your puzzle input).

The rules for valid messages (the top part of your puzzle input) are numbered
and build upon each other. For example:

| 0: 1 2
| 1: "a"
| 2: 1 3 | 3 1
| 3: "b"

Some rules, like 3: "b", simply match a single character (in this case, b).

The remaining rules list the sub-rules that must be followed; for example, the
rule 0: 1 2 means that to match rule 0, the text being checked must match rule
1, and the text after the part that matched rule 1 must then match rule 2.

Some of the rules have multiple lists of sub-rules separated by a pipe (|).
This means that at least one list of sub-rules must match. (The ones that match
might be different each time the rule is encountered.) For example, the rule 2:
1 3 | 3 1 means that to match rule 2, the text being checked must match rule 1
followed by rule 3 or it must match rule 3 followed by rule 1.

Fortunately, there are no loops in the rules, so the list of possible matches
will be finite. Since rule 1 matches a and rule 3 matches b, rule 2 matches
either ab or ba. Therefore, rule 0 matches aab or aba.

Here's a more interesting example:

| 0: 4 1 5
| 1: 2 3 | 3 2
| 2: 4 4 | 5 5
| 3: 4 5 | 5 4
| 4: "a"
| 5: "b"

Here, because rule 4 matches a and rule 5 matches b, rule 2 matches two letters
that are the same (aa or bb), and rule 3 matches two letters that are different
(ab or ba).

Since rule 1 matches rules 2 and 3 once each in either order, it must match two
pairs of letters, one pair with matching letters and one pair with different
letters. This leaves eight possibilities: aaab, aaba, bbab, bbba, abaa, abbb,
baaa, or babb.

Rule 0, therefore, matches a (rule 4), then any of the eight options from rule
1, then b (rule 5): aaaabb, aaabab, abbabb, abbbab, aabaab, aabbbb, abaaab, or
ababbb.

The received messages (the bottom part of your puzzle input) need to be checked
against the rules so you can determine which are valid and which are corrupted.
Including the rules and the messages together, this might look like:

| 0: 4 1 5
| 1: 2 3 | 3 2
| 2: 4 4 | 5 5
| 3: 4 5 | 5 4
| 4: "a"
| 5: "b"
| 
| ababbb
| bababa
| abbbab
| aaabbb
| aaaabbb

Your goal is to determine the number of messages that completely match rule 0.
In the above example, ababbb and abbbab match, but bababa, aaabbb, and aaaabbb
do not, producing the answer 2. The whole message must match all of rule 0;
there can't be extra unmatched characters in the message. (For example, aaaabbb
might appear to match rule 0 above, but it has an extra unmatched b on the
end.)

How many messages completely match rule 0?
-}

import Data.Map(Map,fromList,(!))

data Rx = RxChar Char | Rx [[Rx]] deriving Show

parse :: String -> (Map String Rx,[String])
parse str = (parseRules rules,drop 1 messages)
  where
    (rules,messages) = span (not . null) (lines str)

parseRules :: [String] -> Map String Rx
parseRules rules = rx
  where
    rx = fromList $ map (parseRule . words) rules
    parseRule (name:body) = (filter (/= ':') name,parseBody body)
    parseBody ['"':c:'"':_] = RxChar c
    parseBody subrules = Rx $ parseSubrules subrules
    parseSubrules [] = []
    parseSubrules subrules = map (rx!) firstOption : parseSubrules (drop 1 rest)
      where (firstOption,rest) = span (/= "|") subrules

rxMatch :: Rx -> String -> [String]
rxMatch (RxChar ch) str
  | null str || ch /= head str = []
  | otherwise = [tail str]
rxMatch (Rx options) str = concatMap (rxMatchList [str]) options

rxMatchList :: [String] -> [Rx] -> [String]
rxMatchList strlist [] = strlist
rxMatchList strlist (rx:rxs) = concat [rxMatchList (rxMatch rx str) rxs | str <- strlist]

rxMatchCompletely :: Rx -> String -> Bool
rxMatchCompletely rx str = "" `elem` rxMatch rx str

testData :: String
testData = "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb\n"

test :: ()
test
  | filter (rxMatchCompletely (rxs!"0")) msgs /= ["ababbb","abbbab"] = error "a"
  | otherwise = ()
  where (rxs,msgs) = parse testData

part1 :: IO Int
part1 = do
    (rxs,msgs) <- fmap parse $ readFile "input/19.txt"
    return $ length $ filter (rxMatchCompletely (rxs!"0")) msgs

parse2 :: String -> (Map String Rx,[String])
parse2 str = (parseRules $ map edit rules,drop 1 messages)
  where
    (rules,messages) = span (not . null) (lines str)
    edit "8: 42" = "8: 42 | 42 8"
    edit "11: 42 31" = "11: 42 31 | 42 11 31"
    edit rule = rule

testData2 :: String
testData2 = "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba\n"

test2 :: ()
test2
  | (length . filter (rxMatchCompletely (rxs1!"0"))) msgs /= 3 = error "a"
  | (length . filter (rxMatchCompletely (rxs2!"0"))) msgs /= 12 = error "b"
  | otherwise = ()
  where
    (rxs1,msgs) = parse testData2
    (rxs2,_) = parse2 testData2

part2 :: IO Int
part2 = do
    (rxs,msgs) <- fmap parse2 $ readFile "input/19.txt"
    return $ length $ filter (rxMatchCompletely (rxs!"0")) msgs
