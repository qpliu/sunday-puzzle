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
