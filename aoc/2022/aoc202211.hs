import Data.List(sort)
import Data.Map(Map,adjust,fromList,insert,toList,(!))

parse :: String -> ([(String,(Expr,Int,String,String))],Map String (Int,[Int]))
parse = p [] [] . map words . lines
  where
    p monkeys monkeyStates
        (["Monkey",nameColon]
         :("Starting":"items:":items)
         :("Operation:":"new":"=":expr)
         :["Test:","divisible","by",test]
         :["If","true:","throw","to","monkey",true]
         :["If","false:","throw","to","monkey",false]
         :rest) =
        -- items are reversed for foldr for handling and adding to front after throwing
        p ((name,(parseExpr expr,read test,true,false)):monkeys) ((name,(0,reverse $ map (read . filter (/= ',')) items)):monkeyStates) (drop 1 rest)
      where name = filter (/= ':') nameColon
    p monkeys monkeyStates _ = (reverse monkeys,fromList monkeyStates)

data Expr = ExprOld | ExprNum Int | ExprTimes Expr Expr | ExprPlus Expr Expr deriving Show

parseExpr :: [String] -> Expr
parseExpr tokens = parseLHS tokens
  where
    -- might have more ops or parentheses or have to deal with precedence
    parseLHS ("old":toks) = parseOp ExprOld toks
    parseLHS (tok:toks) = parseOp (ExprNum (read tok)) toks
    parseLHS toks = error (show toks)
    parseOp lhs [] = lhs
    parseOp lhs ("*":toks) = ExprTimes lhs $ parseExpr toks
    parseOp lhs ("+":toks) = ExprPlus lhs $ parseExpr toks
    parseOp lhs toks = error (show (lhs,toks))

eval :: Expr -> Int -> Int
eval ExprOld old = old
eval (ExprNum n) _ = n
eval (ExprTimes lhs rhs) old = eval lhs old * eval rhs old
eval (ExprPlus lhs rhs) old = eval lhs old + eval rhs old

turn :: Map String (Int,[Int]) -> (String,(Expr,Int,String,String)) -> Map String (Int,[Int])
turn monkeyStates (name,(expr,test,true,false)) =
    foldr handleItem (insert name (inspected+length items,[]) monkeyStates) items
  where
    (inspected,items) = monkeyStates!name
    handleItem item ms
      | nextItem `mod` test == 0 = adjust (fmap (nextItem:)) true ms
      | otherwise =  adjust (fmap (nextItem:)) false ms
      where nextItem = eval expr item `div` 3

doRound :: ([(String,(Expr,Int,String,String))],Map String (Int,[Int])) -> ([(String,(Expr,Int,String,String))],Map String (Int,[Int]))
doRound (monkeys,monkeyStates) = (monkeys,foldl turn monkeyStates monkeys)

run :: String -> Int
run = product . take 2 . reverse . sort . map (fst . snd) . toList . snd . head . drop 20 . iterate doRound . parse

testData :: String
testData = unlines [
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
    ]

test :: ()
test
  | run testData /= 10605 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap run $ readFile "input/11.txt"

getMod :: ([(String,(Expr,Int,String,String))],Map String (Int,[Int])) -> Int
getMod ([],_) = 1
getMod ((_,(_,n,_,_)):monkeys,_) = n*getMod (monkeys,undefined)

turn2 :: Int -> Map String (Int,[Int]) -> (String,(Expr,Int,String,String)) -> Map String (Int,[Int])
turn2 m monkeyStates (name,(expr,test,true,false)) =
    foldr handleItem (insert name (inspected+length items,[]) monkeyStates) items
  where
    (inspected,items) = monkeyStates!name
    handleItem item ms
      | nextItem `mod` test == 0 = adjust (fmap (nextItem:)) true ms
      | otherwise =  adjust (fmap (nextItem:)) false ms
      where nextItem = eval expr item `mod` m

doRound2 :: Int -> ([(String,(Expr,Int,String,String))],Map String (Int,[Int])) -> ([(String,(Expr,Int,String,String))],Map String (Int,[Int]))
doRound2 m (monkeys,monkeyStates) = (monkeys,foldl (turn2 m) monkeyStates monkeys)

run2 :: Int -> String -> Int
run2 nRounds input = (product . take 2 . reverse . sort . map (fst . snd) . toList . snd . head . drop nRounds . iterate (doRound2 (getMod state))) state
  where state = parse input

test2 :: ()
test2
  | run2 1 testData /= 4*6 = error "a"
  | run2 20 testData /= 99*103 = error "b"
  | run2 1000 testData /= 5204*5192 = error "c"
  | run2 10000 testData /= 52166*52013 = error "d"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (run2 10000) $ readFile "input/11.txt"
