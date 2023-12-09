import Data.Map(Map,fromList,(!))
import Data.Tuple(swap)

parse :: String -> ([(String,(String,String))],String)
parse = fmap concat . swap . fmap (map parseEdge) . splitAt 2 . lines
  where
    parseEdge = formatEdge . words . filter (not . (`elem` "=(,)"))
    formatEdge [a,b,c] = (a,(b,c))

countSteps :: Map String (String,String) -> (String -> Bool) -> Int -> String -> String -> Int
countSteps graph goal n instructions current
  | goal current = n
  | head instructions == 'L' =
      countSteps graph goal (n+1) (tail instructions) (fst (graph!current))
  | head instructions == 'R' =
      countSteps graph goal (n+1) (tail instructions) (snd (graph!current))

result :: String -> Int
result input = countSteps (fromList graph) ("ZZZ" ==) 0 (cycle instructions) "AAA"
  where (graph,instructions) = parse input

testData :: [String]
testData = [unlines [
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
    ], unlines [
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
    ]]

test :: ()
test
  | map result testData /= [2,6] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/08.txt"

result2 :: String -> Int
result2 input = foldl lcm 1 counts
  where
    (graph,instructions) = parse input
    counts = map (countSteps (fromList graph) ((== 'Z') . last) 0 (cycle instructions)) (filter ((== 'A') . last) (map fst graph))

testData2 :: String
testData2 = unlines [
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
    ]

test2 :: ()
test2
  | result2 testData2 /= 6 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/08.txt"
