-- Looks like my input data does not use tgl, and uses out in only one place.

-- Decompiling my input data, it looks like if the last instruction is
-- executed, the state gets reset to the initial state, so if it outputs
-- one 0,1 pair by the time it is reached, the input is found.

import Data.Map(Map,adjust,fromList,(!))

parse :: String -> ([[String]],[[String]])
parse input = (map words (lines input),[])

registers :: Int -> Map String Int
registers a = fromList [("a",a),("b",0),("c",0),("d",0),("out",1),("count",0)]

isRegister :: String -> Bool
isRegister x = x `elem` ["a","b","c","d"]

interp :: Map String Int -> ([[String]],[[String]]) -> (Bool,Map String Int)
interp regs ([],_) = (False,regs)
interp regs (insn:forward,backward)
  | null forward = (regs!"out" == 1 && regs!"count" > 0,regs)
  | otherwise = interp1 insn
  where
    interp1 ("cpy":x:y:_)
      | isRegister x = interp (adjust (const (regs!x)) y regs) (forward,insn:backward)
      | otherwise = interp (adjust (const (read x)) y regs) (forward,insn:backward)
    interp1 ("inc":x:_) = interp (adjust (+1) x regs) (forward,insn:backward)
    interp1 ("dec":x:_) = interp (adjust (-1+) x regs) (forward,insn:backward)
    interp1 ("jnz":x:y:_)
      | x == "0" || (isRegister x && regs!x == 0) = interp regs (forward,insn:backward)
      | offset < 0 = interp regs (reverse (take (-offset) backward) ++ insn:forward,drop (-offset) backward)
      | otherwise = interp regs (drop offset (insn:forward),reverse (take offset (insn:forward)) ++ backward)
      where offset | isRegister y = regs!y | otherwise = read y
    interp1 ("out":x:_)
      | val /= 1 - regs!"out" = (False,regs)
      | otherwise = interp (adjust (+1) "count" (adjust (const val) "out" regs)) (forward,insn:backward)
      where val | isRegister x = regs!x | otherwise = read x
    interp1 _ = error (show insn)

test :: ()
test
  | (snd $ interp (registers 0) (parse testData))!"a" /= 42 = error "a"
  | otherwise = ()
  where
    testData = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"

search :: ([[String]],[[String]]) -> Int
search insns = head $ filter run [0..]
  where run a = fst $ interp (registers a) insns

part1 :: IO Int
part1 = fmap (search . parse) $ readFile "input/25.txt"
