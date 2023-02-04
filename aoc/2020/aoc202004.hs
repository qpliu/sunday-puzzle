import Data.Char(isDigit)
import Data.Map(Map,empty,insert,keysSet,(!))
import Data.Set(Set,fromList,intersection)

parse :: String -> [Map String String]
parse = p [empty] . map words . lines
  where
    p passports ([]:rest) = p (empty:passports) rest
    p (passport:passports) (fields:rest) = p (foldr update passport fields:passports) rest
    p passports _ = passports
    update field passport = insert k (tail colonv) passport
      where (k,colonv) = span (/= ':') field

required :: Set String
required = fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

valid :: Map String String -> Bool
valid passport = (keysSet passport `intersection` required) == required

testData :: String
testData = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\niyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\nhcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\nhcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n"

test :: ()
test
  | (length . filter valid . parse) testData /= 2 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter valid . parse) $ readFile "input/04.txt"

valid2 :: Map String String -> Bool
valid2 passport
  | not (valid passport) = False
  | invalidNum 1920 2002 (passport!"byr") = False
  | invalidNum 2010 2020 (passport!"iyr") = False
  | invalidNum 2020 2030 (passport!"eyr") = False
  | invalidHgt (passport!"hgt") = False
  | invalidHcl (passport!"hcl") = False
  | invalidEcl (passport!"ecl") = False
  | invalidPid (passport!"pid") = False
  | otherwise = True

invalidNum :: Int -> Int -> String -> Bool
invalidNum low high num = any (not . isDigit) num || n < low || n > high
  where n = read num

invalidHgt :: String -> Bool
invalidHgt hgt
  | units == "cm" = invalidNum 150 193 num
  | units == "in" = invalidNum 59 76 num
  | otherwise = True
  where (num,units) = span isDigit hgt

invalidHcl :: String -> Bool
invalidHcl hcl
  | length hcl /= 7 = True
  | take 1 hcl /= "#" = True
  | otherwise = not $ all (`elem` "0123456789abcdef") (drop 1 hcl)

invalidEcl :: String -> Bool
invalidEcl ecl = not $ elem ecl ["amb","blu","brn","gry","grn","hzl","oth"]

invalidPid :: String -> Bool
invalidPid pid = length pid /= 9 || any (not . isDigit) pid

testData2 :: [String]
testData2 = [
    "eyr:1972 cid:100\nhcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\niyr:2019\nhcl:#602927 eyr:1967 hgt:170cm\necl:grn pid:012533040 byr:1946\n\nhcl:dab227 iyr:2012\necl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\nhgt:59cm ecl:zzz\neyr:2038 hcl:74454a iyr:2023\npid:3556412378 byr:2007\n",
    "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\nhcl:#623a2f\n\neyr:2029 ecl:blu cid:129 byr:1989\niyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\nhcl:#888785\nhgt:164cm byr:2001 iyr:2015 cid:88\npid:545766238 ecl:hzl\neyr:2022\n\niyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"
    ]

test2 :: ()
test2
  | (any valid2 . parse) (testData2 !! 0) = error "a"
  | (not . all valid2 . parse) (testData2 !! 1) = error "b"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (length . filter valid2 . parse) $ readFile "input/04.txt"
