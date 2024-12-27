module AOC202424 where

import Data.Bits(xor,(.&.),(.|.))
import Data.List(intercalate,sort)
import Data.Map(elems,fromList,keys,toList,(!))

import AOC

aoc = AOC {
    day="24",
    aocTests=[
        AOCTest {
            testData=unlines [
                "x00: 1",
                "x01: 1",
                "x02: 1",
                "y00: 0",
                "y01: 1",
                "y02: 0",
                "",
                "x00 AND y00 -> z00",
                "x01 XOR y01 -> z01",
                "x02 OR y02 -> z02"
                ],
            testResult=Just "4",
            testResult2=Nothing
            },
        AOCTest {
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
            testResult=Just "2024",
            testResult2=Nothing
            }
        ],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse,
        codeTest=result,
        codeTest2=undefined,
        codeResult=result,
        codeResult2=result2
        }
    }

data Expr = C Int
          | I String
          | XOR Expr Expr String
          | OR  Expr Expr String
          | AND Expr Expr String
    deriving (Eq,Ord,Show)

parse = map p . filter (not . null) . map words . lines
  where
    p [a,b] = (I (init a),C (read b))
    p [a,"XOR",b,"->",c] = (I c,XOR (I a) (I b) c)
    p [a,"OR", b,"->",c] = (I c,OR  (I a) (I b) c)
    p [a,"AND",b,"->",c] = (I c,AND (I a) (I b) c)

result input = sum $ zipWith (*) (map (table!) (zs table)) [2^n | n <- [0..]]
  where
    table = fromList $ map f input

    f (k,C v) = (k,v)
    f (k,XOR a b _) = (k,(table!a) `xor` (table!b))
    f (k,OR  a b _) = (k,(table!a) .|.   (table!b))
    f (k,AND a b _) = (k,(table!a) .&.   (table!b))

zs table = sort $ filter isZ $ keys table
  where
    isZ (I ('z':_)) = True
    isZ _ = False

prev (I ['z',c,'0']) = (I ['z',pred c,'9'])
prev (I ['z',c,d]) = (I ['z',c,pred d])

x (I ('z':i)) = I ('x':i)
x _ = C 2
y (I ('z':i)) = I ('y':i)
y _ = C 3
toz (I ('x':i)) = I ('z':i)
toz _ = C 4
z00 = I "z00"

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
    f (k,XOR a b i) = kv k XOR a b i
    f (k,OR  a b i) = kv k OR  a b i
    f (k,AND a b i) = kv k AND a b i

    kv k op a b i = (k,op (min aexpr bexpr) (max aexpr bexpr) i)
      where
        aexpr = table!a
        bexpr = table!b

isXOR z (XOR a b _) = a == x z && b == y z
isXOR _ _ = False

isAdd zmax z expr@(XOR a b _)
  | z == z00 = isXOR z expr
  | otherwise = isXOR z a && isCarry (prev z) b
isAdd zmax z expr
  | z == zmax = isCarry (prev z) expr
  | otherwise = False

-- with zNN, just checking that [xNN AND yNN] exists somewhere in
-- the expression is enough for my input
isCarry z (AND a b _)
  | a == x z && b == y z = True
  | otherwise = isCarry z a || isCarry z b
isCarry z (XOR a b _) = isCarry z a || isCarry z b
isCarry z (OR a b _) = isCarry z a || isCarry z b
isCarry z _ = False

findSwap input swaps = check $ zs table
  where
    table = makeTable swaps input
    zmax = maximum $ zs table

    xorXYs = fromList [(z,expr) | expr <- elems table, z <- zForXORxy expr]
    zForXORxy (XOR x1 y1 _)
      | x1 == x (toz x1) && y1 == y (toz x1) = [toz x1]
      | otherwise = []
    zForXORxy _ = []

    addXYs = fromList [(z,expr) | expr <- elems table, z <- zForAddXY expr]
    zForAddXY (XOR (XOR x1 y1 _) _ _)
      | x1 == x (toz x1) && y1 == y (toz x1) = [toz x1]
      | otherwise = []
    zForAddXY (XOR _ (XOR x1 y1 _) _)
      | x1 == x (toz x1) && y1 == y (toz x1) = [toz x1]
      | otherwise = []
    zForAddXY _ = []

    check [] = []
    check (z@(I zi):rest)
      | z == z00 && table!z == xorXYs!z = check rest
      | z == zmax = checkCarry (prev z) (table!z)
      | otherwise = checkAdd (table!z)
      where
        checkAdd (XOR a b _)
          | a == xorXY = checkCarry (prev z) b
          | b == xorXY = checkCarry (prev z) a
          | isCarry (prev z) a = makeSwap b
          | isCarry (prev z) b = makeSwap a
          | otherwise = error (show z)
          where
            xorXY@(XOR _ _ xorXYi) = xorXYs!z
            makeSwap (XOR _ _ i) = [i,xorXYi]
            makeSwap (OR _ _ i) = [i,xorXYi]
            makeSwap (AND _ _ i) = [i,xorXYi]
        checkAdd _ = zi : [ai | (XOR _ _ ai) <- [addXYs!z]]

        checkCarry zc _ = check rest -- not needed for my input

result2 input = format $ swaps []
  where
    swaps swapList
      | null swap = []
      | otherwise = swap : swaps (swap:swapList)
      where swap = findSwap input swapList
    format = intercalate "," . sort . concat
