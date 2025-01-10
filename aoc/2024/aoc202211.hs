module AOC202211 where

import Data.List(partition,sort)
import Data.Map(Map,adjust,elems,fromList,(!))

import AOC

aoc = AOC {
    day="../../2022/input/11",
    aocTests=[
        AOCTest {
            testData=unlines [
                "Monkey 0:",
                "  Starting items: 79, 98",
                "  Operation: new = old * 19",
                "  Test: divisible by 23",
                "    If true: throw to monkey 2",
                "    If false: throw to monkey 3",
                "",
                "Monkey 1:",
                "  Starting items: 54, 65, 75, 74",
                "  Operation: new = old + 6",
                "  Test: divisible by 19",
                "    If true: throw to monkey 2",
                "    If false: throw to monkey 0",
                "",
                "Monkey 2:",
                "  Starting items: 79, 60, 97",
                "  Operation: new = old * old",
                "  Test: divisible by 13",
                "    If true: throw to monkey 1",
                "    If false: throw to monkey 3",
                "",
                "Monkey 3:",
                "  Starting items: 74",
                "  Operation: new = old + 3",
                "  Test: divisible by 17",
                "    If true: throw to monkey 0",
                "    If false: throw to monkey 1"
                ],
            testResult=Just "10605",
            testResult2=Just "2713310158"
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

type Monkey = (String,Int -> Int,Int -> Bool,String,String)

parse :: String -> ([Monkey],(Map String Int,Map String [Int]))
parse = p 1 [] [] . lines
  where
    p mm items monkeys [] =
        (map (makeManageable mm) monkeys,
         (fromList (map (fmap (const 0)) items),fromList items))
    p mm items monkeys
      (monkeyHeader:startingItems:operation:test:ifTrue:ifFalse:rest) =
        p (lcm mm ndiv) ((monkeyId,startItems):items) (monkey:monkeys)
        $ dropWhile null rest
      where
        monkey = (monkeyId,op,tst,trueMonkey,falseMonkey)

        monkeyId = parseMonkeyId $ words monkeyHeader
        startItems = parseInts startingItems
        op = parseOp $ words operation
        [ndiv] = parseInts test
        tst = (== 0) . (`mod` ndiv)
        trueMonkey = parseIf $ words ifTrue
        falseMonkey = parseIf $ words ifFalse

        parseMonkeyId ["Monkey",monkeyColon] = init monkeyColon
        parseOp ["Operation:","new","=","old",op,"old"]
          | op == "*" = (^ 2)
          | op == "+" = (* 2)
        parseOp ["Operation:","new","=","old",op,n]
          | op == "*" = (* read n)
          | op == "+" = (+ read n)
        parseIf ["If",_,"throw","to","monkey",m] = m
    makeManageable mm (monkeyId,op,tst,trueMonkey,falseMonkey) =
         (monkeyId,(`mod` mm) . op,tst,trueMonkey,falseMonkey)

monkeyRound :: (Int -> Int) -> [Monkey] -> (Map String Int,Map String [Int])
                                        -> (Map String Int,Map String [Int])
monkeyRound relief monkeys state = foldr (monkeyTurn relief) state monkeys

monkeyTurn :: (Int -> Int) -> Monkey -> (Map String Int,Map String [Int])
                                     -> (Map String Int,Map String [Int])
monkeyTurn relief (monkeyId,op,test,true,false) (counts,monkeyItems) =
    (adjust (length items +) monkeyId counts,nextMonkeyItems)
  where
    items = map (relief . op) $ monkeyItems!monkeyId
    (toTrue,toFalse) = partition test items
    nextMonkeyItems =
        adjust (const []) monkeyId $ adjust (++ toTrue) true
                                   $ adjust (++ toFalse) false monkeyItems

monkeyBusiness :: (Map String Int,Map String [Int]) -> Int
monkeyBusiness (counts,_) = product $ take 2 $ reverse $ sort $ elems counts

result (monkeys,state) =
    monkeyBusiness $ head $ drop 20
                   $ iterate (monkeyRound (`div` 3) monkeys) state

result2 (monkeys,state) =
    monkeyBusiness $ head $ drop 10000
                   $ iterate (monkeyRound id monkeys) state
