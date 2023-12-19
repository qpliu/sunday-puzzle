import Data.Char(isDigit)
import Data.List(groupBy,nub,sort)
import Data.Map(Map,elems,fromList,(!))

type Part = [Int]
type Rule = (Maybe (Char,Char,Int),String)

pieces :: String -> [String]
pieces = filter (not . (`elem` ["{",",","}"])) . groupBy (\ c1 c2 -> not (c1 `elem` "{,}") && not (c2 `elem` "{,}"))

rulePieces :: String -> [String]
rulePieces = groupBy (\ c1 c2 -> not (c1 `elem` "<>:") && not (c2 `elem` "<>:"))

parse :: String -> (Map String [Rule],[Part])
parse = p . span (/= "") . lines
  where
    p (flows,parts) = (fromList $ map (parseFlow . pieces) flows,map (parsePart . pieces) $ drop 1 parts)
    parseFlow (name:rules) = (name,map (parseRule . rulePieces) rules)
    parseRule [[rating],[test],n,":",dest] = (Just (rating,test,read n),dest)
    parseRule [dest] = (Nothing,dest)
    parsePart = map (read . filter isDigit)

accept :: Map String [Rule] -> String -> Part -> Bool
accept flows flow part@[x,m,a,s] = apply (flows!flow)
  where
    apply ((test,"A"):rest)
      | check test = True
      | otherwise = apply rest
    apply ((test,"R"):rest)
      | check test = False
      | otherwise = apply rest
    apply ((test,newFlow):rest)
      | check test = accept flows newFlow part
      | otherwise = apply rest
    check Nothing = True
    check (Just ('x','>',n)) = x > n
    check (Just ('x','<',n)) = x < n
    check (Just ('m','>',n)) = m > n
    check (Just ('m','<',n)) = m < n
    check (Just ('a','>',n)) = a > n
    check (Just ('a','<',n)) = a < n
    check (Just ('s','>',n)) = s > n
    check (Just ('s','<',n)) = s < n

result :: String -> Int
result input = sum $ map sum $ filter (accept flows "in") parts
  where
    (flows,parts) = parse input

testData :: String
testData = unlines [
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
    ]

test :: ()
test
  | result testData /= 19114 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap result $ readFile "input/19.txt"

accept2 :: String -> ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Map String [Rule] -> [((Int,Int),(Int,Int),(Int,Int),(Int,Int))]
accept2 flow ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) flows = apply (flows!flow) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax))
  where
    apply _ ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) | xmin >= xmax || mmin >= mmax || amin >= amax || smin >= smax = []
    apply ((Nothing,nextFlow):_) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = continue nextFlow ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax))
    apply ((Just ('x','>',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,min (n+1) xmax),(mmin,mmax),(amin,amax),(smin,smax))) ++ (continue nextFlow ((max xmin (n+1),xmax),(mmin,mmax),(amin,amax),(smin,smax)))
    apply ((Just ('x','<',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((max xmin n,xmax),(mmin,mmax),(amin,amax),(smin,smax))) ++ (continue nextFlow ((xmin,min n xmax),(mmin,mmax),(amin,amax),(smin,smax)))
    apply ((Just ('m','>',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(mmin,min (n+1) mmax),(amin,amax),(smin,smax))) ++ (continue nextFlow ((xmin,xmax),(max mmin (n+1),mmax),(amin,amax),(smin,smax)))
    apply ((Just ('m','<',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(max mmin n,mmax),(amin,amax),(smin,smax))) ++ (continue nextFlow ((xmin,xmax),(mmin,min n mmax),(amin,amax),(smin,smax)))
    apply ((Just ('a','>',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(mmin,mmax),(amin,min (n+1) amax),(smin,smax))) ++ (continue nextFlow ((xmin,xmax),(mmin,mmax),(max amin (n+1),amax),(smin,smax)))
    apply ((Just ('a','<',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(mmin,mmax),(max amin n,amax),(smin,smax))) ++ (continue nextFlow ((xmin,xmax),(mmin,mmax),(amin,min n amax),(smin,smax)))
    apply ((Just ('s','>',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,min (n+1) smax))) ++ (continue nextFlow ((xmin,xmax),(mmin,mmax),(amin,amax),(max smin (n+1),smax)))
    apply ((Just ('s','<',n),nextFlow):rules) ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) = (apply rules ((xmin,xmax),(mmin,mmax),(amin,amax),(max smin n,smax))) ++ (continue nextFlow ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,min n smax)))
    continue _  ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax)) | xmin >= xmax || mmin >= mmax || amin >= amax || smin >= smax = []
    continue "R" ranges = []
    continue "A" ranges = [ranges]
    continue flow ranges = accept2 flow ranges flows

rangeSize :: ((Int,Int),(Int,Int),(Int,Int),(Int,Int)) -> Int
rangeSize ((xmin,xmax),(mmin,mmax),(amin,amax),(smin,smax))
  | xmax > xmin && mmax > mmin && amax > amin && smax > smin = (xmax-xmin)*(mmax-mmin)*(amax-amin)*(smax-smin)
  | otherwise = 0

result2 :: String -> Int
result2 = sum . map rangeSize . accept2 "in" ((1,4001),(1,4001),(1,4001),(1,4001)) . fst . parse

test2 :: ()
test2
  | result2 testData /= 167409079868000 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap result2 $ readFile "input/19.txt"
