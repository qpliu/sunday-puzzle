import Data.Map(Map,fromList,(!))

parse :: String -> Map String Int
parse input = table
  where
    table = fromList $ map (p . words) $ lines input
    toName = filter (/= ':')
    p [name, num] = (toName name,read num)
    p [name, aaaa, "+", bbbb] = (toName name,table!aaaa + table!bbbb)
    p [name, aaaa, "-", bbbb] = (toName name,table!aaaa - table!bbbb)
    p [name, aaaa, "*", bbbb] = (toName name,table!aaaa * table!bbbb)
    p [name, aaaa, "/", bbbb] = (toName name,table!aaaa `div` table!bbbb)

testData :: String
testData = unlines [
    "root: pppw + sjmn",
    "dbpl: 5",
    "cczh: sllz + lgvd",
    "zczc: 2",
    "ptdq: humn - dvpt",
    "dvpt: 3",
    "lfqf: 4",
    "humn: 5",
    "ljgn: 2",
    "sjmn: drzm * dbpl",
    "sllz: 4",
    "pppw: cczh / lfqf",
    "lgvd: ljgn * ptdq",
    "drzm: hmdt - zczc",
    "hmdt: 32"
    ]

test :: ()
test
  | ((!"root") . parse) testData /= 152 = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap ((!"root") . parse) $ readFile "input/21.txt"

data Expr = Hum | Num Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Root Expr Expr deriving Show

parse2 :: String -> Map String Expr
parse2 input = table
  where
    table = fromList $ map (p . words) $ lines input
    toName = filter (/= ':')
    p ["root:", aaaa, _, bbbb] = ("root",Root (table!aaaa) (table!bbbb))
    p ["humn:", _] = ("humn",Hum)
    p [name, num] = (toName name,Num (read num))
    p [name, aaaa, "+", bbbb] = (toName name,addE (table!aaaa) (table!bbbb))
    p [name, aaaa, "-", bbbb] = (toName name,subE (table!aaaa) (table!bbbb))
    p [name, aaaa, "*", bbbb] = (toName name,mulE (table!aaaa) (table!bbbb))
    p [name, aaaa, "/", bbbb] = (toName name,divE (table!aaaa) (table!bbbb))
    addE (Num a) (Num b) = Num (a+b)
    addE a b = Add a b
    subE (Num a) (Num b) = Num (a-b)
    subE a b = Sub a b
    mulE (Num a) (Num b) = Num (a*b)
    mulE a b = Mul a b
    divE (Num a) (Num b) = Num (a `div` b)
    divE a b = Div a b

invert :: Expr -> Int
invert (Root (Num a) b) = invert (Root b (Num a))
invert (Root Hum (Num a)) = a
invert (Root (Add (Num a) b) (Num c)) = invert (Root b (Num (c - a)))
invert (Root (Sub (Num a) b) (Num c)) = invert (Root b (Num (a - c)))
invert (Root (Mul (Num a) b) (Num c)) = invert (Root b (Num (c `div` a)))
invert (Root (Div (Num a) b) (Num c)) = invert (Root b (Num (a `div` c)))
invert (Root (Add a (Num b)) (Num c)) = invert (Root a (Num (c - b)))
invert (Root (Sub a (Num b)) (Num c)) = invert (Root a (Num (c + b)))
invert (Root (Mul a (Num b)) (Num c)) = invert (Root a (Num (c `div` b)))
invert (Root (Div a (Num b)) (Num c)) = invert (Root a (Num (c*b)))

test2 :: ()
test2
  | (invert . (!"root") . parse2) testData /= 301 = error "a"
  | otherwise = ()

part2 :: IO Int
part2 = fmap (invert . (!"root") . parse2) $ readFile "input/21.txt"
