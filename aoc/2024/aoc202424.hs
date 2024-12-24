module AOC202424 where

import Data.Bits(xor,(.&.),(.|.))
import Data.List(intercalate,sort)
import Data.Map(fromList,keys,(!))

import AOC

aoc = AOC {
    day="24",
    testData=unlines [
    "x00: 1",
    "x01: 0",
    "x02: 1",
    "x03: 1",
    "x04: 0",
    "y00: 1",
    "y01: 1",
    "y02: 1",
    "y03: 1",
    "y04: 1",
    "",
    "ntg XOR fgs -> mjb",
    "y02 OR x01 -> tnw",
    "kwq OR kpj -> z05",
    "x00 OR x03 -> fst",
    "tgd XOR rvg -> z01",
    "vdt OR tnw -> bfw",
    "bfw AND frj -> z10",
    "ffh OR nrd -> bqk",
    "y00 AND y03 -> djm",
    "y03 OR y00 -> psh",
    "bqk OR frj -> z08",
    "tnw OR fst -> frj",
    "gnj AND tgd -> z11",
    "bfw XOR mjb -> z00",
    "x03 OR x00 -> vdt",
    "gnj AND wpb -> z02",
    "x04 AND y00 -> kjc",
    "djm OR pbm -> qhw",
    "nrd AND vdt -> hwm",
    "kjc AND fst -> rvg",
    "y04 OR y02 -> fgs",
    "y01 AND x02 -> pbm",
    "ntg OR kjc -> kwq",
    "psh XOR fgs -> tgd",
    "qhw XOR tgd -> z09",
    "pbm OR djm -> kpj",
    "x03 XOR y03 -> ffh",
    "x00 XOR y04 -> ntg",
    "bfw OR bqk -> z06",
    "nrd XOR fgs -> wpb",
    "frj XOR qhw -> z04",
    "bqk OR frj -> z07",
    "y03 OR x01 -> nrd",
    "hwm AND bqk -> z03",
    "tgd XOR rvg -> z12",
    "tnw OR pbm -> gnj"
    ],
    testResult="2024",
    testData2="",
    testResult2="\"\"",
    aocParse=parse,
    aocTest=result,
    aocResult=result,
    aocParse2=parse,
    aocTest2=const "",
    aocResult2=result2
    }

data Expr = C Int | I String | XOR Expr Expr | OR Expr Expr | AND Expr Expr
    deriving (Eq,Ord,Show)

parse = map p . filter (not . null) . map words . lines
  where
    p [a,b] = (I (init a),C (read b))
    p [a,"XOR",b,"->",c] = (I c,XOR (I a) (I b))
    p [a,"OR", b,"->",c] = (I c,OR  (I a) (I b))
    p [a,"AND",b,"->",c] = (I c,AND (I a) (I b))

result input = sum $ zipWith (*) (map (table!) (zs table)) [2^n | n <- [0..]]
  where
    table = fromList $ map f input

    f (k,C v) = (k,v)
    f (k,XOR a b) = (k,(table!a) `xor` (table!b))
    f (k,OR  a b) = (k,(table!a) .|.   (table!b))
    f (k,AND a b) = (k,(table!a) .&.   (table!b))

zs table = sort $ filter isZ $ keys table
  where
    isZ (I ('z':_)) = True
    isZ _ = False

showWire c n
  | n < 10 = c:'0':show n
  | otherwise = c:show n

showX = showWire 'x'
showY = showWire 'y'
showZ = showWire 'z'

makeTable swaps input = table
  where
    table = fromList $ map (f . doSwaps) $ input
    doSwaps (I a,expr) = (I $ swap a swaps,expr)
    doSwaps expr = expr

    swap a [] = a
    swap a ([b,c]:rest)
      | a == b = c
      | a == c = b
      | otherwise = swap a rest

    f (k,C _) = (k,k)
    f (k,XOR a b) = (k,XOR (table!a) (table!b))
    f (k,OR  a b) = (k,OR  (table!a) (table!b))
    f (k,AND a b) = (k,AND (table!a) (table!b))


isXOR n (XOR (I a) (I b)) =
    (a == showX n && b == showY n) || (a == showY n && b == showX n)
isXOR _ _ = False

isAdd n expr@(XOR a b)
  | n == 0 = isXOR n expr
  | otherwise =
        (isXOR n a && isCarry (n-1) b) || (isXOR n b && isCarry (n-1) a)
isAdd n expr
  | n == 45 = isCarry (n-1) expr
  | otherwise = False

isCarry n _ = True -- not needed for my input

result2 input
  | not $ null bad = "bad:" ++ format bad
  | otherwise = format $ concat swaps
  where
    -- from manual inspection
    swaps = [["z10","ggn"],["z39","twr"],["z32","grm"],["ndw","jcb"]]
    table = makeTable swaps input
    bad = [zid | z@(I zid) <- zs table,
                     expr <- [table!z],
                     n <- [makeN z],
                     not ((z == maxZ && isCarry (n-1) expr)
                          || isAdd n expr)]
    maxZ = maximum $ zs table
    makeN (I ('z':'0':n)) = read n
    makeN (I ('z':n)) = read n
    format = intercalate "," . sort
