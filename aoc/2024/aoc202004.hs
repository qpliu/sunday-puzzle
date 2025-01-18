module AOC202004 where

import Data.Char(isDigit)
import Data.Map(Map,fromList,member,(!))

import AOC

aoc = AOC {
    day="../../2020/input/04",
    aocTests=[
        AOCTest {
            testData=unlines [
                "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd",
                "byr:1937 iyr:2017 cid:147 hgt:183cm",
                "",
                "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884",
                "hcl:#cfa07d byr:1929",
                "",
                "hcl:#ae17e1 iyr:2013",
                "eyr:2024",
                "ecl:brn pid:760753108 byr:1931",
                "hgt:179cm",
                "",
                "hcl:#cfa07d eyr:2025 pid:166559648",
                "iyr:2011 ecl:brn hgt:59in"
                ],
            testResult=Just "2",
            testResult2=Nothing
            },
        AOCTest {
            testData=unlines [
                "eyr:1972 cid:100",
                "hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926",
                "",
                "iyr:2019",
                "hcl:#602927 eyr:1967 hgt:170cm",
                "ecl:grn pid:012533040 byr:1946",
                "",
                "hcl:dab227 iyr:2012",
                "ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277",
                "",
                "hgt:59cm ecl:zzz",
                "eyr:2038 hcl:74454a iyr:2023",
                "pid:3556412378 byr:2007"
                ],
            testResult=Nothing,
            testResult2=Just "0"
            },
        AOCTest {
            testData=unlines [
                "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980",
                "hcl:#623a2f",
                "",
                "eyr:2029 ecl:blu cid:129 byr:1989",
                "iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm",
                "",
                "hcl:#888785",
                "hgt:164cm byr:2001 iyr:2015 cid:88",
                "pid:545766238 ecl:hzl",
                "eyr:2022",
                "",
                "iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719"
                ],
            testResult=Nothing,
            testResult2=Just "4"
            }
        ],
    aocCode=ParallelCode {
        pcodeParse=const parse,
        pcodeParse2=const parse,
        pcodeTest=result,
        pcodeTest2=result2,
        pcodeResult=result,
        pcodeResult2=result2
        }
    }

parse = map (parseRecord . concatMap words) . parseBlankLineSeparated
  where  
    parseRecord = fromList . map parseField
    parseField field = (name,value)
      where (name,':':value) = span (/= ':') field

valid :: Map String String -> Int
valid record
  | all (`member` record) ["byr","iyr","eyr","hgt","hcl","ecl","pid"] = 1
  | otherwise = 0

result ncpu = parallelMapReduce ncpu valid sum

valid2 :: Map String String -> Int
valid2 record
  | valid record == 0 = 0
  | invalidYear (record!"byr") 1920 2002 = 0  
  | invalidYear (record!"iyr") 2010 2020 = 0  
  | invalidYear (record!"eyr") 2020 2030 = 0  
  | invalidHeight (record!"hgt") = 0
  | invalidHairColor (record!"hcl") = 0
  | invalidEyeColor (record!"ecl") = 0
  | invalidPassportID (record!"pid") = 0
  | otherwise = 1

invalidYear :: String -> Int -> Int -> Bool
invalidYear year minYr maxYr
  | length year /= 4 || any (not . isDigit) year = True
  | otherwise = yr < minYr || maxYr < yr
  where yr = read year

invalidHeight :: String -> Bool
invalidHeight height
  | null hgt = True
  | units == "in" && 59 <= h && h <= 76 = False
  | units == "cm" && 150 <= h && h <= 193 = False
  | otherwise = True
  where
    (hgt,units) = span isDigit height
    h = read hgt

invalidHairColor :: String -> Bool
invalidHairColor ('#':hcl) =
    length hcl /= 6 || not (all (`elem` "0123456789abcdef") hcl)
invalidHairColor _ = True

invalidEyeColor :: String -> Bool
invalidEyeColor = not . (`elem` ["amb","blu","brn","gry","grn","hzl","oth"])

invalidPassportID :: String -> Bool
invalidPassportID pid = length pid /= 9 || not (all isDigit pid)

result2 ncpu = parallelMapReduce ncpu valid2 sum
