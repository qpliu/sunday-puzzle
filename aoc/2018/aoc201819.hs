-- I'll assume #ip declarations apply to the instructions that follow until
-- the next #ip declaration.

import Data.Array(Array,bounds,inRange,listArray,(!),(//))
import Data.Bits((.&.),(.|.))
import Data.List(group)

type Insn = (Int,String,(Int,Int,Int))
type State = (Int,Array Int Int)

parse :: String -> Array Int Insn
parse s = listArray (0,length insns - 1) insns
  where
    insns = p (-1) $ words s
    p bip ("#ip":ip:rest) = p (read ip) rest
    p bip (insn:a:b:c:rest) = (bip,insn,(read a,read b,read c)) : p bip rest
    p _ _ = []

initState :: State
initState = (0,listArray (-1,5) [0,0,0,0,0,0,0])

step :: Array Int Insn -> State -> Either State State
step insns state@(ip,regs)
  | not (inRange (bounds insns) ip) = Left state
  | ip == 35 || ip == 26 = Left state
  | otherwise = Right (1+regs2!bip,regs2)
  where
    insn@(bip,op,operands) = insns!ip
    regs1 = regs // [(bip,ip)]
    regs2 = exec op operands regs1

exec :: String -> (Int,Int,Int) -> Array Int Int -> Array Int Int
exec op (a,b,c) regs
  | op == "addr" = regs // [(c,regs!a + regs!b)]
  | op == "addi" = regs // [(c,regs!a + b)]
  | op == "mulr" = regs // [(c,regs!a * regs!b)]
  | op == "muli" = regs // [(c,regs!a * b)]
  | op == "banr" = regs // [(c,regs!a .&. regs!b)]
  | op == "bani" = regs // [(c,regs!a .&. b)]
  | op == "borr" = regs // [(c,regs!a .|. regs!b)]
  | op == "bori" = regs // [(c,regs!a .|. b)]
  | op == "setr" = regs // [(c,regs!a)]
  | op == "seti" = regs // [(c,a)]
  | op == "gtir" = regs // [(c,if a > regs!b then 1 else 0)]
  | op == "gtri" = regs // [(c,if regs!a > b then 1 else 0)]
  | op == "gtrr" = regs // [(c,if regs!a > regs!b then 1 else 0)]
  | op == "eqir" = regs // [(c,if a == regs!b then 1 else 0)]
  | op == "eqri" = regs // [(c,if regs!a == b then 1 else 0)]
  | op == "eqrr" = regs // [(c,if regs!a == regs!b then 1 else 0)]

run :: Array Int Insn -> State -> State
run insns state = either id (run insns) (step insns state)

testData :: String
testData = "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"

test :: ()
test
  | flip run initState (parse testData) /= (7,listArray (-1,5) [0,6,5,6,0,0,9]) = error "a"
  | otherwise = ()

part1 :: IO Int
part1 =
    fmap ((!0) . snd . flip run initState . parse) $ readFile "input/19.txt"

initState2 :: State
initState2 = (0,listArray (-1,5) [0,1,0,0,0,0,0])

-- My input has the r4 bound to ip and the initialization code setting
-- r0=0 r1=0 r2=989 r3=0 r5=62 when r0=0
-- r0=0 r1=0 r2=10551298 r3=0 r5=10550400

-- Decompiling my input, it looks like r0 is the sum of all numbers that
-- divide r2.

primes :: [Int]
primes = sieve [2..]
  where
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

factors :: Int -> [Int]
factors n = f primes n []
  where
    f [] _ _ = []
    f (p:ps) n facts
      | p > n = facts
      | n `mod` p /= 0 = f ps n facts
      | otherwise = f (p:ps) (n `div` p) (p:facts)

divisors :: Int -> [Int]
divisors n = d (group $ factors n)
  where
    d [] = [1]
    d (ns@(n:_):rest) = [ n^p*f | p <- [0..length ns], f <- d rest]

part2 :: [Int]
part2 = map (sum . divisors) [898,10551298]
