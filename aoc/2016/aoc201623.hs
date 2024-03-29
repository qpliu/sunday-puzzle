import Data.Map(Map,adjust,fromList,(!))

parse :: String -> ([[String]],[[String]])
parse input = (map words (lines input),[])

registers :: Map String Int
registers = fromList [("a",7),("b",0),("c",0),("d",0)]

isRegister :: String -> Bool
isRegister x = x `elem` ["a","b","c","d"]

interp :: Map String Int -> ([[String]],[[String]]) -> Map String Int
interp regs ([],_) = regs
interp regs (insn:forward,backward) = interp1 insn
  where
    interp1 ("cpy":x:y:_)
      | not (isRegister y) = interp regs (forward,insn:backward)
      | isRegister x = interp (adjust (const (regs!x)) y regs) (forward,insn:backward)
      | otherwise = interp (adjust (const (read x)) y regs) (forward,insn:backward)
    interp1 ("inc":x:_)
      | isRegister x = interp (adjust (+1) x regs) (forward,insn:backward)
      | otherwise = interp regs (forward,insn:backward)
    interp1 ("dec":x:_)
      | isRegister x = interp (adjust (-1+) x regs) (forward,insn:backward)
      | otherwise = interp regs (forward,insn:backward)
    interp1 ("jnz":x:y:_)
      | x == "0" || (isRegister x && regs!x == 0) = interp regs (forward,insn:backward)
      | offset < 0 = interp regs (reverse (take (-offset) backward) ++ insn:forward,drop (-offset) backward)
      | otherwise = interp regs (drop offset (insn:forward),reverse (take offset (insn:forward)) ++ backward)
      where offset | isRegister y = regs!y | otherwise = read y
    interp1 ("tgl":x:_)
      | offset < 0 = interp regs (forward,tgl (1-offset) (insn:backward))
      | otherwise = interp regs (tgl offset forward,insn:backward)
      where offset | isRegister x = regs!x | otherwise = read x
    interp1 _ = error (show insn)

tgl :: Int -> [[String]] -> [[String]]
tgl offset insns = take (offset-1) insns ++ tgl1 (drop (offset-1) insns)
  where
    tgl1 [] = []
    tgl1 (("cpy":args):rest) = (("jnz":args):rest)
    tgl1 (("inc":args):rest) = (("dec":args):rest)
    tgl1 (("dec":args):rest) = (("inc":args):rest)
    tgl1 (("jnz":args):rest) = (("cpy":args):rest)
    tgl1 (("tgl":args):rest) = (("inc":args):rest)
    tgl1 (insn:_) = error (show insn)

test :: ()
test
  | (interp registers (parse testData1))!"a" /= 42 = error "a"
  | (interp registers (parse testData2))!"a" /= 3 = error "b"
  | otherwise = ()
  where
    testData1 = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"
    testData2 = "cpy 2 a\ntgl a\ntgl a\ntgl a\ncpy 1 a\ndec a\ndec a"

part1 :: IO Int
part1 = fmap ((!"a") . interp registers . parse) $ readFile "input/23.txt"

-- Part 2 involves hand decompiling my input.

insn1 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn1 (a,b,c,d) = insn3 (a,a-1,c,d)

insn3 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn3 (a,b,c,d) = insn5 (0,b,c,a)

insn5 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn5 (a,b,c,d) = insn11 (b*d,b,0,0)

insn11 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn11 (a,b,c,d) = insn14 (a,b-1,b-1,b-1)

insn14 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn14 (a,b,c,d) = insn17 (a,b,c+d,0)

-- insn17 is the tgl instruction,
-- which, when c = 8,6,4 toggles insns 25,23,21 into adding 80*84 to a
-- and, when c = 2, toggles insn19 from looping back to insn3 to setting c = 1
insn17 :: (Int,Int,Int,Int) -> (Int,Int,Int,Int)
insn17 (a,b,c,d)
  | c > 2 = insn3 (a,b,-16,d)
  | otherwise = (a+80*84,b,0,0)

part2 :: Int -> (Int,Int,Int,Int)
part2 a = insn1 (a,0,0,0)
