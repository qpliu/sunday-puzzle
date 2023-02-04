import Data.Map((!))
import qualified Data.Map
import Data.Set(Set,fromList,intersection,size)

parse :: String -> [([Set Char],[Set Char])]
parse = p . map fromList . words
  where
    p [] = []
    p items = (take 10 items,drop 11 (take 15 items)) : p (drop 15 items)

is1478 :: Set a -> Bool
is1478 item = size item `elem` [2,4,3,7]

testData :: String
testData = "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n"

test :: ()
test
  | (length . filter is1478 . concatMap snd . parse) testData /= 26 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter is1478 . concatMap snd . parse) $ readFile "input/08.txt"

decode :: ([Set Char],[Set Char]) -> String
decode (digits,output) = map (table!) output
  where
    one = head $ filter ((== 2) . size) digits
    seven = head $ filter ((== 3) . size) digits
    four = head $ filter ((== 4) . size) digits
    d235 = filter ((== 5) . size) digits
    d069 = filter ((== 6) . size) digits
    eight = head $ filter ((== 7) . size) digits
    three = head $ filter ((== 2) . size . intersection one) d235
    two = head $ filter ((== 2) . size . intersection four) d235
    five = head $ filter (/= two) $ filter (/= three) d235
    six = head $ filter ((/= one) . intersection one) d069
    nine = head $ filter ((== four) . intersection four) d069
    zero = head $ filter ((/= five) . intersection five) d069
    table = Data.Map.fromList [(zero,'0'),(one,'1'),(two,'2'),(three,'3'),(four,'4'),(five,'5'),(six,'6'),(seven,'7'),(eight,'8'),(nine,'9')]

test2 :: ()
test2
  | (sum . map (read . decode) . parse) testData /= 61229 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (sum . map (read . decode) . parse) $ readFile "input/08.txt"
