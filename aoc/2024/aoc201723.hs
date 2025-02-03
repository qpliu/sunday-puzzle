module AOC201723 where

import Data.Array(Array,array,bounds,inRange)
import qualified Data.Array
import Data.Map(Map,adjust,fromList,insert,(!))

import AOC

aoc = AOC {
    day="../../2017/input/23",
    aocTests=[],
    aocCode=Code {
        codeParse=parse,
        codeParse2=parse2,
        codeTest=result,
        codeTest2=result2,
        codeResult=result,
        codeResult2=result2
        }
    }

type State = (Int,(Int,Map String Int))

parseInsn :: [String] -> State -> State
parseInsn [op,x,y]
  | op == "set" && null yval =
      \ (muls,(ip,regs)) -> (muls,(ip+1,insert x (regs!y) regs))
  | op == "set" =
      \ (muls,(ip,regs)) -> (muls,(ip+1,insert x (head yval) regs))
  | op == "sub" && null yval =
      \ (muls,(ip,regs)) -> (muls,(ip+1,adjust (+ (-regs!y)) x regs))
  | op == "sub" =
      \ (muls,(ip,regs)) -> (muls,(ip+1,adjust (+ (-head yval)) x regs))
  | op == "mul" && null yval =
      \ (muls,(ip,regs)) -> (muls+1,(ip+1,adjust (* (regs!y)) x regs))
  | op == "mul" =
      \ (muls,(ip,regs)) -> (muls+1,(ip+1,adjust (* (head yval)) x regs))
  | op == "jnz" && null xval && null yval =
      \ (muls,(ip,regs)) -> if regs!x /= 0
                              then (muls,(ip+regs!y,regs))
                              else (muls,(ip+1,regs))
  | op == "jnz" && null xval =
      \ (muls,(ip,regs)) -> if regs!x /= 0
                              then (muls,(ip+head yval,regs))
                              else (muls,(ip+1,regs))
  | op == "jnz" && head xval == 0 && null yval =
      \ (muls,(ip,regs)) -> (muls,(ip+regs!y,regs))
  | op == "jnz" && head xval == 0 =
      \ (muls,(ip,regs)) -> (muls,(ip+1,regs))
  | op == "jnz" =
      \ (muls,(ip,regs)) -> (muls,(ip+head yval,regs))
  where
    xval = parseInts x
    yval = parseInts y

parse input = array (0,length insns-1) $ zip [0..] insns
  where insns = map (parseInsn . words) $ lines input

interp :: State -> Array Int (State -> State) -> Int
interp state@(muls,(ip,_)) insns
  | not $ inRange (bounds insns) ip = muls
  | otherwise = interp ((insns Data.Array.!ip) state) insns

result = interp (0,(0,fromList $ zip (map (:[]) ['a'..'h']) (repeat 0)))

{-
Decompiling:

b=parameter A                  - parameter A at 0
c=b
if a then
  b=b*parameter B+parameter C  - parameter B at 4, parameter C at 5
  c=b+parameter D              - parameter D at 7
end if
loop
  f=1
  for d in 2..b
    for e in 2..b
      if d*e == b then
        f=0
      end if
    end for
  end for
  if f then
    h=h+1
  end if
  if b == c then
    halt
  end if
  b=b+parameter E              - parameter E at 30
end loop
-}

parse2 = foldr collect (0,0,0,0,0) . zip [0..] . map parseInts . lines
  where
    collect (0,[a]) (_,b,c,d,e) = (a,b,c,d,e)
    collect (4,[b]) (a,_,c,d,e) = (a,b,c,d,e)
    collect (5,[c]) (a,b,_,d,e) = (a,b,-c,d,e)
    collect (7,[d]) (a,b,c,_,e) = (a,b,c,-d,e)
    collect (30,[e]) (a,b,c,d,_) = (a,b,c,d,-e)
    collect _ params = params

primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

notPrime :: Int -> Bool
notPrime n = f primes
  where
    f (p:ps)
      | r == 0 = q > 1
      | q < p = False
      | otherwise = f ps
      where (q,r) = n `divMod` p

result2 (a,b,c,d,e) = length $ filter notPrime [a*b+c,a*b+c+e .. a*b+c+d]
