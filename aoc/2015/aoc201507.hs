{-
--- Day 7: Some Assembly Required ---

This year, Santa brought little Bobby Tables a set of wires and bitwise logic
gates! Unfortunately, little Bobby is a little under the recommended age range,and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit
signal (a number from 0 to 65535). A signal is provided to each wire by a gate,
another wire, or some specific value. Each wire can only get a signal from one
source, but can provide its signal to multiple destinations. A gate provides no
signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together:
x AND y -> z means to connect wires x and y to an AND gate, and then connect
its output to wire z.

For example:

 - 123 -> x means that the signal 123 is provided to wire x.
 - x AND y -> z means that the bitwise AND of wire x and wire y is provided to
   wire z.
 - p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and
   then provided to wire q.
 - NOT e -> f means that the bitwise complement of the value from wire e is
   provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for
some reason, you'd like to emulate the circuit instead, almost all programming
languages (for example, C, JavaScript, or Python) provide operators for these
gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

In little Bobby's kit's instructions booklet (provided as your puzzle input),
what signal is ultimately provided to wire a?
-}
import Data.Bits(shiftL,shiftR,xor,(.&.),(.|.))
import Data.Map(Map,fromList,(!))
import Data.Word(Word16)

interpret :: Map String Word16 -> String -> (String,Word16)
interpret circuit instruction = interp (words instruction)
  where
    interp (x:"->":y:_) = (y,eval x)
    interp (x:"AND":y:"->":z:_) = (z,eval x .&. eval y)
    interp (x:"OR":y:"->":z:_) = (z,eval x .|. eval y)
    interp (x:"LSHIFT":y:"->":z:_) = (z,eval x `shiftL` (fromIntegral (eval y)))
    interp (x:"RSHIFT":y:"->":z:_) = (z,eval x `shiftR` (fromIntegral (eval y)))
    interp ("NOT":x:"->":y:_) = (y,eval x `xor` 65535)
    interp _ = error ("unknown instruction:"++instruction)
    eval x
      | head x `elem` "0123456789" = read x
      | otherwise = circuit!x

makeCircuit :: String -> Map String Word16
makeCircuit instructions = circuit
  where
    circuit = fromList $ map (interpret circuit) $ lines instructions

main :: IO ()
main = getContents >>= print . (!"a") . makeCircuit

test :: ()
test
  | circuit!"d" /= 72 = error "a"
  | circuit!"e" /= 507 = error "b"
  | circuit!"f" /= 492 = error "c"
  | circuit!"g" /= 114 = error "d"
  | circuit!"h" /= 65412 = error "e"
  | circuit!"i" /= 65079 = error "f"
  | circuit!"x" /= 123 = error "g"
  | circuit!"y" /= 456 = error "h"
  | otherwise = ()
  where
    circuit = makeCircuit "123 -> x\n456 -> y\nx AND y -> d\nx OR y -> e\nx LSHIFT 2 -> f\ny RSHIFT 2 -> g\nNOT x -> h\nNOT y -> i"

part1 :: IO Word16
part1 = fmap ((!"a") . makeCircuit) (readFile "input/07.txt")

interp2 :: Word16 -> Map String Word16 -> String -> (String,Word16)
interp2 part1a circuit instruction
  | take 2 (reverse $ words instruction) == ["b","->"] = ("b",part1a)
  | otherwise = interpret circuit instruction

part2makeCircuit :: Word16 -> String -> Map String Word16
part2makeCircuit part1a instructions = circuit
  where
    circuit = fromList $ map (interp2 part1a circuit) $ lines instructions

part2 :: IO Word16
part2 = do
  part1a <- part1
  fmap ((!"a") . part2makeCircuit part1a) (readFile "input/07.txt")
