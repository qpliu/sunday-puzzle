{-
--- Day 25: Clock Signal ---

You open the door and find yourself on the roof. The city sprawls away from you
for miles and miles.

There's not much time now - it's already Christmas, but you're nowhere near the
North Pole, much too far to deliver these stars to the sleigh in time.

However, maybe the huge antenna up here can offer a solution. After all, the
sleigh doesn't need the stars, exactly; it needs the timing data they provide,
and you happen to have a massive signal generator right here.

You connect the stars you have to your prototype computer, connect that to the
antenna, and begin the transmission.

Nothing happens.

You call the service number printed on the side of the antenna and quickly
explain the situation. "I'm not sure what kind of equipment you have connected
over there," he says, "but you need a clock signal." You try to explain that
this is a signal for a clock.

"No, no, a clock signal - timing information so the antenna computer knows how
to read the data you're sending it. An endless, alternating pattern of 0, 1, 0,
1, 0, 1, 0, 1, 0, 1...." He trails off.

You ask if the antenna can handle a clock signal at the frequency you would
need to use for the data from the stars. "There's no way it can! The only
antenna we've installed capable of that is on top of a top-secret Easter Bunny
installation, and you're definitely not-" You hang up the phone.

You've extracted the antenna's clock signal generation assembunny code (your
puzzle input); it looks mostly compatible with code you worked on just
recently.

This antenna code, being a signal generator, uses one extra instruction:

 - out x transmits x (either an integer or the value of a register) as the next
   value for the clock signal.

The code takes a value (via register a) that describes the signal to generate,
but you're not sure how it's used. You'll have to find the input to produce the
right signal through experimentation.

What is the lowest positive integer that can be used to initialize register a
and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?
-}

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
