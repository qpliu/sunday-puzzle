import Data.Map(Map,adjust,fromList,(!))

parse :: String -> ([[String]],[[String]])
parse input = (map words (lines input),[])

registers :: Map String Int
registers = fromList [("a",0),("b",0),("c",0),("d",0)]

isRegister :: String -> Bool
isRegister x = x `elem` ["a","b","c","d"]

interp :: Map String Int -> ([[String]],[[String]]) -> Map String Int
interp regs ([],_) = regs
interp regs (insn:forward,backward) = interp1 insn
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
    interp1 _ = error (show insn)

test :: ()
test
  | (interp registers (parse testData))!"a" /= 42 = error "a"
  | otherwise = ()
  where
    testData = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a"

part1 :: IO Int
part1 = fmap ((!"a") . interp registers . parse) $ readFile "input/12.txt"

part2registers :: Map String Int
part2registers = fromList [("a",0),("b",0),("c",1),("d",0)]

part2 :: IO Int
part2 = fmap ((!"a") . interp part2registers . parse) $ readFile "input/12.txt"
