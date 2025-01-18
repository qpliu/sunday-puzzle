module AOC202007 where

import Data.Map(Map,empty,fromList,insert,keys,member,size,toList,(!))

import AOC

aoc = AOC {
    day="../../2020/input/07",
    aocTests=[
        AOCTest {
            testData=unlines [
                "light red bags contain 1 bright white bag, 2 muted yellow bags.",
                "dark orange bags contain 3 bright white bags, 4 muted yellow bags.",
                "bright white bags contain 1 shiny gold bag.",
                "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.",
                "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.",
                "dark olive bags contain 3 faded blue bags, 4 dotted black bags.",
                "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.",
                "faded blue bags contain no other bags.",
                "dotted black bags contain no other bags."
                ],
            testResult=Just "4",
            testResult2=Just "32"
            },
        AOCTest {
            testData=unlines [
                "shiny gold bags contain 2 dark red bags.",
                "dark red bags contain 2 dark orange bags.",
                "dark orange bags contain 2 dark yellow bags.",
                "dark yellow bags contain 2 dark green bags.",
                "dark green bags contain 2 dark blue bags.",
                "dark blue bags contain 2 dark violet bags.",
                "dark violet bags contain no other bags."
                ],
            testResult=Nothing,
            testResult2=Just "126"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse = map (p . words) . lines
  where
    p [a1,a2,"bags","contain","no","other","bags."] = ((a1,a2),empty)
    p (a1:a2:"bags":"contain":others) = ((a1,a2),fromList (parseOthers others))
    parseOthers [] = []
    parseOthers (n:a1:a2:_:rest) = ((a1,a2),read n :: Int) : parseOthers rest

result rules = size (count empty [("shiny","gold")]) - 1
  where
    count set [] = set
    count set (color:queue)
      | member color set = count set queue
      | otherwise = count (insert color () set) (containers ++ queue)
      where containers = map fst $ filter (member color . snd) $ rules

result2 rules = table!("shiny","gold")
  where
    table = fromList $ map toTable rules
    toTable (color,contents) =
        (color,sum (map countContents $ toList contents))
    countContents (color,count) = count*(1+table!color)
