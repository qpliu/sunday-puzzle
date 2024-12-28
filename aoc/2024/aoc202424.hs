module AOC202424 where

import Data.Bits(xor,(.&.),(.|.))
import Data.List(intercalate,sort)
import Data.Map(Map,alter,empty,findWithDefault,fromList,insert,member,(!))
import Data.Set(Set)
import qualified Data.Set

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
            },
        AOCTest {
            testData=unlines [
                "x00: 0",
                "x01: 1",
                "x02: 0",
                "x03: 1",
                "x04: 0",
                "x05: 1",
                "y00: 0",
                "y01: 0",
                "y02: 1",
                "y03: 1",
                "y04: 0",
                "y05: 1",
                "",
                "x00 AND y00 -> z05",
                "x01 AND y01 -> z02",
                "x02 AND y02 -> z01",
                "x03 AND y03 -> z03",
                "x04 AND y04 -> z04",
                "x05 AND y05 -> z00"
                ],
            testResult=Nothing,
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

data Op = IN Int
        | XOR String String String
        | OR  String String String
        | AND String String String
    deriving (Eq,Ord,Show)

parse :: String -> [(String,Op)]
parse = map p . filter (not . null) . map words . lines
  where
    p [a,b] = (init a,IN $ read b)
    p [a,"XOR",b,"->",c] = (c,XOR a b c)
    p [a,"OR", b,"->",c] = (c,OR  a b c)
    p [a,"AND",b,"->",c] = (c,AND a b c)

zs :: [(String,Op)] -> [String]
zs = sort . filter ((== 'z') . head) . map fst

result input = sum $ zipWith (*) (map (table!) $ zs input) [2^n | n <- [0..]]
  where
    table = fromList $ map (fmap op) input
    op (IN n) = n
    op (XOR a b _) = table!a `xor` table!b
    op (OR  a b _) = table!a .|.   table!b
    op (AND a b _) = table!a .&.   table!b

tabulate :: [(String,Op)]
         -> (Map String [Op],Map String [Op],Map String [Op],
             Map String Op,Set String)
tabulate = foldr collect (empty,empty,empty,empty,Data.Set.empty)
  where
    collect (_,op@(XOR a b c)) (xors,ors,ands,outs,ins) =
        (add op a b xors,ors,ands,insert c op outs,ins)
    collect (_,op@(OR  a b c)) (xors,ors,ands,outs,ins) =
        (xors,add op a b ors,ands,insert c op outs,ins)
    collect (_,op@(AND a b c)) (xors,ors,ands,outs,ins) =
        (xors,ors,add op a b ands,insert c op outs,ins)
    collect (v,_) (xors,ors,ands,outs,ins) =
        (xors,ors,ands,outs,Data.Set.insert v ins)
    add op a b =
        alter (Just . maybe [op] (op:)) a . alter (Just . maybe [op] (op:)) b

x00 = "x00"
y00 = "y00"
z00 = "z00"
next [c,tens,'9'] = [c,succ tens,'0']
next [c,tens,ones] = [c,tens,succ ones]

swapList :: (Map String [Op],Map String [Op],Map String [Op],
             Map String Op,Set String)
         -> [String]
swapList (xors,ors,ands,outs,ins) = checkAdd00
  where
    addSwap :: String -> String -> Map String String -> Map String String
    addSwap a b swaps = insert a b $ insert b a swaps
    swap :: String -> Map String String -> String
    swap a swaps = findWithDefault a a swaps

    checkZ :: String -> String -> Map String String -> Bool
    checkZ z out swaps = checkZout (outs!z)
      where
        checkZout (XOR _ _ outz) = out == swap outz swaps
        checkZout _ = False

    checkAdd00 :: [String]
    checkAdd00
      | xorX00 /= xorY00 = error (show (xorX00,xorY00))
      | checkZ z00 xorX00 empty = checkCarry00 empty
      | otherwise = z00 : xorX00 : checkCarry00 (addSwap z00 xorX00 empty)
      where
        (XOR _ _ xorX00:_) = xors!x00
        (XOR _ _ xorY00:_) = xors!y00

    checkCarry00 :: Map String String -> [String]
    checkCarry00 swaps
      | andX00 /= andY00 = error (show (andX00,andY00))
      | otherwise = checkAdd (next x00) (next y00) andX00 (next z00) swaps
      where
        (AND _ _ andX00:_) = ands!x00
        (AND _ _ andY00:_) = ands!y00

    checkAdd :: String -> String -> String -> String -> Map String String
             -> [String]
    checkAdd x y carry z swaps
      | not (Data.Set.member x ins) && z == carry = []
      | not (Data.Set.member x ins) = [carry,z]
        -- only (x XOR y) XOR carry in my input
        -- no cases of (x XOR carry) XOR y in my input
        -- no cases of (y XOR carry) XOR x in my input
      | swap xorX swaps /= swap xorY swaps = undefined
      | not (member (swap xorX swaps) xors)
              && xxory2 == swap carry swaps =
          carry2:xorX:checkAdd x y carry z (addSwap carry2 xorX swaps)
      | not (member (swap xorX swaps) xors)
              && carry2 == swap carry swaps =
          xxory2:xorX:checkAdd x y carry z (addSwap xxory2 xorX swaps)
      | not (member (swap carry swaps) xors)
              && xxory1 == swap carry swaps =
          carry1:carry:checkAdd x y carry z (addSwap carry1 carry swaps)
      | not (member (swap carry swaps) xors)
              && carry1 == swap carry swaps =
          xxory1:carry:checkAdd x y carry z (addSwap xxory1 carry swaps)
      | swap xorX swaps == swap xorY swaps
              && swap xorXorX swaps == swap xorCarry swaps
              && checkZ z xorXorX swaps =
          checkCarry x y carry z swaps
      | swap xorX swaps == swap xorY swaps
              && swap xorXorX swaps == swap xorCarry swaps =
          z:xorXorX:checkCarry x y carry z (addSwap z xorXorX swaps)
      | otherwise = undefined
      where
        (XOR _ _ xorX:_) = xors!x
        (XOR _ _ xorY:_) = xors!y
        (XOR xxory1 carry1 xorXorX:_) = xors!swap xorX swaps
        (XOR xxory2 carry2 xorCarry:_) = xors!swap carry swaps

    checkCarry :: String -> String -> String -> String -> Map String String
               -> [String]
    checkCarry x y carry z swaps
        -- only ((x XOR y) AND carry) OR (x AND y) in my input
        -- no cases of ((x XOR carry) AND y) OR (x AND carry) in my input
        -- no cases of ((y XOR carry) AND x) OR (y AND carry) in my input
      | swap xorX swaps /= swap xorY swaps = undefined
      | swap andX swaps /= swap andY swaps = undefined
      | swap andXorX swaps == swap andCarry swaps
              && swap andX swaps == swap andY swaps
              && swap orAndXorX swaps == swap orAndX swaps =
          checkAdd (next x) (next y) orAndX (next z) swaps
      | otherwise = undefined
      where
        (XOR _ _ xorX:_) = xors!x
        (XOR _ _ xorY:_) = xors!y
        (AND _ _ andXorX:_) = ands!swap xorX swaps
        (AND _ _ andCarry:_) = ands!swap carry swaps
        (AND _ _ andX:_) = ands!x
        (AND _ _ andY:_) = ands!y
        (OR _ _ orAndXorX:_) = ors!swap andXorX swaps
        (OR _ _ orAndX:_) = ors!swap andX swaps

result2 = sort . swapList . tabulate
