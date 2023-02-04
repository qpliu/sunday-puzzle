import Data.Array(Array,array,bounds,inRange,(!))

data Insn = Hlfa | Hlfb | Tpla | Tplb | Inca | Incb | Jmp Int | Jiea Int | Jieb Int | Jioa Int | Jiob Int deriving Show

exec :: (Int,(Integer,Integer)) -> Array Int Insn -> (Int,(Integer,Integer))
exec state@(pc,_) program
  | inRange (bounds program) pc = exec (exec1 (program!pc) state) program
  | otherwise = state

exec1 :: Insn -> (Int,(Integer,Integer)) -> (Int,(Integer,Integer))
exec1 Hlfa (pc,(a,b)) = (pc+1,(a `div` 2,b))
exec1 Hlfb (pc,(a,b)) = (pc+1,(a,b `div` 2))
exec1 Tpla (pc,(a,b)) = (pc+1,(a*3,b))
exec1 Tplb (pc,(a,b)) = (pc+1,(a,b*3))
exec1 Inca (pc,(a,b)) = (pc+1,(a+1,b))
exec1 Incb (pc,(a,b)) = (pc+1,(a,b+1))
exec1 (Jmp offset) (pc,(a,b)) = (pc+offset,(a,b))
exec1 (Jiea offset) (pc,(a,b)) = (pc+if a `mod` 2 == 0 then offset else 1,(a,b))
exec1 (Jieb offset) (pc,(a,b)) = (pc+if b `mod` 2 == 0 then offset else 1,(a,b))
exec1 (Jioa offset) (pc,(a,b)) = (pc+if a == 1 then offset else 1,(a,b))
exec1 (Jiob offset) (pc,(a,b)) = (pc+if b == 1 then offset else 1,(a,b))

parse :: String -> Array Int Insn
parse s = array (1,length insns) (zip [1..] insns)
  where
    insns = map (parseLine . words) (lines s)
    parseLine ("hlf":"a":_) = Hlfa
    parseLine ("hlf":"b":_) = Hlfb
    parseLine ("tpl":"a":_) = Tpla
    parseLine ("tpl":"b":_) = Tplb
    parseLine ("inc":"a":_) = Inca
    parseLine ("inc":"b":_) = Incb
    parseLine ("jmp":('+':offset):_) = Jmp (read offset)
    parseLine ("jmp":('-':offset):_) = Jmp (0 - read offset)
    parseLine ("jie":"a,":('+':offset):_) = Jiea (read offset)
    parseLine ("jie":"a,":('-':offset):_) = Jiea (0 - read offset)
    parseLine ("jie":"b,":('+':offset):_) = Jieb (read offset)
    parseLine ("jie":"b,":('-':offset):_) = Jieb (0 - read offset)
    parseLine ("jio":"a,":('+':offset):_) = Jioa (read offset)
    parseLine ("jio":"a,":('-':offset):_) = Jioa (0 - read offset)
    parseLine ("jio":"b,":('+':offset):_) = Jiob (read offset)
    parseLine ("jio":"b,":('-':offset):_) = Jiob (0 - read offset)

main :: IO ()
main = getContents >>= print . snd . snd . exec (1,(0,0)) . parse

test :: ()
test
  | exec (1,(0,0)) (parse "inc a\njio a, +2\ntpl a\ninc a") /= (5,(2,0)) = error "a"
  | otherwise = ()

part1 :: IO Integer
part1 = fmap (snd . snd . exec (1,(0,0)) . parse) (readFile "input/23.txt")

part2 :: IO Integer
part2 = fmap (snd . snd . exec (1,(1,0)) . parse) (readFile "input/23.txt")
