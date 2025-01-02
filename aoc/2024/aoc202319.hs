module AOC202319 where

import Data.Map(Map,fromList,(!))

import AOC

aoc = AOC {
    day="../../2023/input/19",
    aocTests=[
        AOCTest {
            testData=unlines [
                "px{a<2006:qkq,m>2090:A,rfg}",
                "pv{a>1716:R,A}",
                "lnx{m>1548:A,A}",
                "rfg{s<537:gd,x>2440:R,A}",
                "qs{s>3448:A,lnx}",
                "qkq{x<1416:A,crn}",
                "crn{x>2662:A,R}",
                "in{s<1351:px,qqz}",
                "qqz{s>2770:qs,m<1801:hdj,R}",
                "gd{a>3333:R,R}",
                "hdj{m>838:A,pv}",
                "",
                "{x=787,m=2655,a=1222,s=2876}",
                "{x=1679,m=44,a=2067,s=496}",
                "{x=2036,m=264,a=79,s=2244}",
                "{x=2461,m=1339,a=466,s=291}",
                "{x=2127,m=1623,a=2188,s=1013}"
                ],
            testResult=Just "19114",
            testResult2=Just "167409079868000"
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

parse :: String -> ([Int] -> Bool,[[Int]])
parse input = (table!"in",map parseInts $ drop 1 parts)
  where
    (workflows,parts) = span (not . null) $ lines input
    table = fromList $ ("A",const True):("R",const False)
                                       :map parseWorkflow workflows

    parseWorkflow workflow = (name,parseRules rules)
      where (name,rules) = span (/= '{') workflow

    parseRules (_:'x':rest) | isCond rest = parseRule1 head rest
    parseRules (_:'m':rest) | isCond rest = parseRule1 (head . drop 1) rest
    parseRules (_:'a':rest) | isCond rest = parseRule1 (head . drop 2) rest
    parseRules (_:'s':rest) | isCond rest = parseRule1 (head . drop 3) rest
    parseRules (_:label) = table!init label

    isCond (c:_) = c == '>' || c == '<'

    parseRule1 select ('>':rest) = parseRule2 select (>) rest
    parseRule1 select ('<':rest) = parseRule2 select (<) rest
    parseRule1 select a = error a

    parseRule2 select op rules part
      | op (select part) n = (table!label) part
      | otherwise = parseRules rest2 part
      where
        (number,rest) = span (/= ':') rules
        n = read number
        (label,rest2) = span (/= ',') $ drop 1 rest

result :: ([Int] -> Bool,[[Int]]) -> Int
result (workflow,parts) = sum $ map sum $ filter workflow parts

parse2 :: String -> [(Int,Int)] -> [[(Int,Int)]]
parse2 input = table!"in"
  where
    workflows = takeWhile (not . null) $ lines input
    table =
        fromList $ ("A",(:[])) : ("R",const []) : map parseWorkflow workflows

    parseWorkflow workflow = (name,parseRules rules)
      where (name,rules) = span (/= '{') workflow

    parseRules (_:'x':rest) | isCond rest = parseRule 0 rest
    parseRules (_:'m':rest) | isCond rest = parseRule 1 rest
    parseRules (_:'a':rest) | isCond rest = parseRule 2 rest
    parseRules (_:'s':rest) | isCond rest = parseRule 3 rest
    parseRules (_:label) = table!init label

    isCond (c:_) = c == '>' || c == '<'

    parseRule i (op:rest) = rule i op n label (parseRules rest2)
      where
        (number,rest1) = span (/= ':') rest
        n = read number
        (label,rest2) = span (/= ',') $ drop 1 rest1

    rule i '<' n label rules part
      | lo >= n = rules part
      | hi < n = (table!label) part
      | otherwise = (table!label) (replace i part (lo,n-1))
                         ++ rules (replace i part (n,hi))
      where (lo,hi) = get i part
    rule i '>' n label rules part
      | hi <= n = rules part
      | lo > n = (table!label) part
      | otherwise = rules (replace i part (lo,n))
         ++ (table!label) (replace i part (n+1,hi))
      where (lo,hi) = get i part

    get i part = head $ drop i part
    replace i part new = take i part ++ new : drop (i+1) part

result2 :: ([(Int,Int)] -> [[(Int,Int)]]) -> Int
result2 workflow = sum $ map count $ workflow $ replicate 4 (1,4000)
  where count = product . map ((+1) . abs . uncurry (-))
