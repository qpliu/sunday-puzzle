{-
--- Day 6: Signals and Noise ---

Something is jamming your communications with Santa. Fortunately, your signal
is only partially jammed, and protocol in situations like this is to switch to
a simple repetition code to get the message through.

In this model, the same message is sent repeatedly. You've recorded the
repeating message signal (your puzzle input), but the data seems quite
corrupted - almost too badly to recover. Almost.

All you need to do is figure out which character is most frequent for each
position. For example, suppose you had recorded the following messages:

eedadn
drvtee
eandsr
raavrd
atevrs
tsrnev
sdttsa
rasrtv
nssdts
ntnada
svetve
tesnvt
vntsnd
vrdear
dvrsen
enarar

The most common character in the first column is e; in the second, a; in the
third, s, and so on. Combining these characters returns the error-corrected
message, easter.

Given the recording in your puzzle input, what is the error-corrected version
of the message being sent?
-}

import Data.List(group,sort)

ec :: [String] -> String
ec msgs
  | null (head msgs) = ""
  | otherwise = mostCommon (map head msgs) : ec (map tail msgs)

mostCommon :: String -> Char
mostCommon s = snd $ maximum $ map (\ l -> (length l,head l)) $ group $ sort s

test :: ()
test
  | ec (lines "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar") /= "easter" = error "a"
  | otherwise = ()

part1 :: IO String
part1 = fmap (ec . lines) $ readFile "input/06.txt"

ec2 :: [String] -> String
ec2 msgs
  | null (head msgs) = ""
  | otherwise = leastCommon (map head msgs) : ec2 (map tail msgs)

leastCommon :: String -> Char
leastCommon s = snd $ minimum $ map (\ l -> (length l,head l)) $ group $ sort s

test2 :: ()
test2
  | ec2 (lines "eedadn\ndrvtee\neandsr\nraavrd\natevrs\ntsrnev\nsdttsa\nrasrtv\nnssdts\nntnada\nsvetve\ntesnvt\nvntsnd\nvrdear\ndvrsen\nenarar") /= "advent" = error "a"
  | otherwise = ()

part2 :: IO String
part2 = fmap (ec2 . lines) $ readFile "input/06.txt"
