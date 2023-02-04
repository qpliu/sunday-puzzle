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
