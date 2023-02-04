import Data.Bits((.&.),(.|.))
import Data.Char(isDigit)
import Data.Map(Map,adjust,fromList,insert,mapWithKey,toList,(!))
import qualified Data.Map
import Data.Set(Set,difference,intersection,unions,size)
import qualified Data.Set

readNum :: String -> Int
readNum = read . filter isDigit

type Sample = ([Int],Int,(Int,Int,Int),[Int])

parse :: String -> ([Sample],[Int])
parse = psamples [] . words
  where
    psamples samples ("Before:":rb1:rb2:rb3:rb4:op:a:b:c:"After:":ra1:ra2:ra3:ra4:rest) =
        psamples (([readNum rb1,readNum rb2,readNum rb3,readNum rb4],readNum op,(readNum a,readNum b,readNum c),[readNum ra1,readNum ra2,readNum ra3,readNum ra4]):samples) rest
    psamples samples rest = (reverse samples,map readNum rest)

type Insn = Map Int Int -> (Int,Int,Int) -> Map Int Int

insns :: [(String,Insn)]
insns = [
    ("addr",\ regs (a,b,c) -> insert c (regs!a + regs!b) regs),
    ("addi",\ regs (a,b,c) -> insert c (regs!a + b) regs),
    ("mulr",\ regs (a,b,c) -> insert c (regs!a * regs!b) regs),
    ("muli",\ regs (a,b,c) -> insert c (regs!a * b) regs),
    ("banr",\ regs (a,b,c) -> insert c (regs!a .&. regs!b) regs),
    ("bani",\ regs (a,b,c) -> insert c (regs!a .&. b) regs),
    ("borr",\ regs (a,b,c) -> insert c (regs!a .|. regs!b) regs),
    ("bori",\ regs (a,b,c) -> insert c (regs!a .|. b) regs),
    ("setr",\ regs (a,b,c) -> insert c (regs!a) regs),
    ("seti",\ regs (a,b,c) -> insert c a regs),
    ("gtir",\ regs (a,b,c) -> insert c (if a > regs!b then 1 else 0) regs),
    ("gtri",\ regs (a,b,c) -> insert c (if regs!a > b then 1 else 0) regs),
    ("gtrr",\ regs (a,b,c) -> insert c (if regs!a > regs!b then 1 else 0) regs),
    ("eqir",\ regs (a,b,c) -> insert c (if a == regs!b then 1 else 0) regs),
    ("eqri",\ regs (a,b,c) -> insert c (if regs!a == b then 1 else 0) regs),
    ("eqrr",\ regs (a,b,c) -> insert c (if regs!a == regs!b then 1 else 0) regs)
    ]

match :: Sample -> Insn -> Bool
match (before,_,operands,after) insn =
    insn (fromList (zip [0..] before)) operands == (fromList (zip [0..] after))

testData :: String
testData = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]"

matches3 :: Sample -> Bool
matches3 sample = length (filter (match sample . snd) insns) >= 3

test :: ()
test
  | map fst (filter (match (head $ fst $ parse testData) . snd) insns) /= ["addi","mulr","seti"] = error "a"
  | otherwise = ()

part1 :: IO Int
part1 = fmap (length . filter matches3 . fst . parse) $ readFile "input/16.txt"

initScan :: Map Int (Set String)
initScan = fromList $ zip [0..15] $ repeat allInsns
  where allInsns = Data.Set.fromList (map fst insns)

scanSample :: Sample -> Map Int (Set String) -> Map Int (Set String)
scanSample sample@(_,op,_,_) m =
    adjust (intersection matchedInsns) op m
  where
    matchedInsns = Data.Set.fromList (map fst $ filter (match sample . snd) insns)

propogateKnowns :: Map Int (Set String) -> Map Int String
propogateKnowns m
  | hasUnknown = propogateKnowns $ mapWithKey removeSingles m
  | otherwise = Data.Map.map minimum m
  where
    hasUnknown = any ((> 1) . size) m
    removeSingles op insnNames = insnNames `difference` singles op
    singles op = unions [insnName | (op2,insnName) <- toList m, op2 /= op, size insnName == 1]

toInsnTable :: Map Int String -> Map Int Insn
toInsnTable = Data.Map.map ((fromList insns)!)

initRegs :: Map Int Int
initRegs = fromList $ zip [0..3] $ repeat 0

interp :: Map Int Insn -> Map Int Int -> [Int] -> Map Int Int
interp insnTable regs (op:a:b:c:rest) = interp insnTable ((insnTable!op) regs (a,b,c)) rest
interp insnTable regs _ = regs

run2 :: String -> Map Int Int
run2 input = interp insnTable initRegs code
  where
    (samples,code) = parse input
    insnTable = toInsnTable $ propogateKnowns $ foldr scanSample initScan samples

part2 :: IO Int
part2 = fmap ((!0) . run2) $ readFile "input/16.txt"
